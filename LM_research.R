#loading libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(patchwork)
library(descr)
library(boot)



#reading date from Stat Ca
labour<-read.table(file='data/pub0325.txt',header=TRUE,sep="\t",stringsAsFactors = TRUE)
#indicating columns as factors
names<-as.vector(colnames(labour),mode="character")
names_factor<-setdiff(names,c("REC_NUM","FINALWT","PAIDOT","UNPAIDOT",
        "XTRAHRS","WKSAWAY","UHRSMAIN","AHRSMAIN",
        "UTOTHRS","ATOTHRS","HRSAWAY","TENURE","PREVTEN","HRLYEARN"))

# Factor columns are                  "SURVYEAR","SURVMNTH","LFSSTAT",
#                                     "PROV","CMA","AGE_12","AGE_6","GENDER",
#                                     "MARSTAT","EDUC","MJH","EVERWORK","FTPTLAST","COWMAIN","IMMIG",
#                                     "NAICS_21","NOC_10","NOC_43","YABSENT","PAYAWAY",
#                                     "FTPTMAIN","YAWAY","WHYPT","UNION",
#                                     "PERMTEMP","ESTSIZE","FIRMSIZE",
#                                     "DURUNEMP","FLOWUNEM","UNEMFTPT","WHYLEFTO","WHYLEFTN",
#                                     "AVAILABL","LKPUBAG","LKEMPLOY","LKRELS","LKATADS","LKANSADS",
#                                     "LKOTHERN","PRIORACT","YNOLOOK","TLOLOOK", 
#                                     "SCHOOLN","EFAMTYPE","AGYOWNK"

labour[names_factor] <- lapply(labour[names_factor], as.factor)
summary(labour)
#rename factor names for immigrant status (IMMIG) and gender (GENDER)
labour$IMMIG<-revalue(labour$IMMIG, c("1" = "Immigrant,not more 10y", "2" = "Immigrant,more 10y", "3"="Non-immigrant"))
labour$GENDER <- revalue(labour$GENDER, c("1" = "Male", "2" = "Female"))
labour$FTPTMAIN<-revalue(labour$FTPTMAIN,c("1"="Full-time","2"="Part-time"))
# labour$WHYPT<-revalue(labour$WHYPT,c("0"="Other reasons",
#                                      "1"=	"Own illness or disability",
#                                      "2"=	"Caring for children",
#                                      "3"=	"Other personal or family responsibilities",
#                                      "4"=	"Going to school",
#                                      "5"=	"Personal preference",
#                                      "6"=	"Business conditions or could not find full-time work, looked for full-time work in last month",
#                                      "7"=	"Business conditions or could not find full-time work, did not look for full-time work in last month"
# ))


#means for extra hours depending on gender
tapply(labour$XTRAHRS, labour$GENDER, mean, na.rm = TRUE)
#full-time vs part-time and gender
table(labour$GENDER,labour$FTPTMAIN)
# reasons for part-time vs gender
tab<-xtabs(~ GENDER + WHYPT, data=labour )
tab
#Conclusion : women stay away from work taking care of the chieldren approx 10x then men

#prop.table(tab,1)
#mean(labour$XTRAHRS,na.rm = TRUE)

#hour rate* usual hours=income  vs type of employment
tapply(0.01*labour$HRLYEARN, labour$PERMTEMP, mean, na.rm = TRUE)
tapply(0.01*labour$HRLYEARN, labour$PERMTEMP , median, na.rm = TRUE)

#hour rate* usual hours=income  vs gender

tapply(0.01*labour$HRLYEARN*labour$UTOTHRS, labour$GENDER , mean, na.rm = TRUE)
tapply(0.01*labour$HRLYEARN*labour$UTOTHRS, labour$GENDER , median, na.rm = TRUE)
boxplot(HRLYEARN ~ GENDER ,data=labour, ylim=c(0,15000))


#hour rate* usual hours=income  vs immigrant status

#First let us check proportion of immigrants in the survey

summary(labour$IMMIG)

tapply(0.01*labour$HRLYEARN*labour$UTOTHRS, labour$IMMIG , mean, na.rm = TRUE)
tapply(0.01*labour$HRLYEARN*labour$UTOTHRS, labour$IMMIG , summary, na.rm = TRUE)
boxplot(0.01*HRLYEARN*UTOTHRS ~ IMMIG ,data=labour)
boxplot(HRLYEARN ~ IMMIG ,data=labour)
boxplot(UTOTHRS ~ IMMIG ,data=labour)

tapply(labour$HRLYEARN, labour$AGE_12 , mean, na.rm = TRUE)

ggplot(data=labour, aes(x=IMMIG,y=0.01*HRLYEARN*UTOTHRS))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_minimal() +
  labs(x="Immigration status",y="Salary")

#idea: apply Leslie model, study the age structure of population with non-immigrants only
#use old data and check Leslie model prediction

# AVERAGE SALARY 
# EXTRA HOURS by GENDER and AGE
tmp<-labour %>% with(tapply(0.01*HRLYEARN*UTOTHRS,AGE_12,mean, na.rm=TRUE))
tmp

plot(tmp)

#hyst plot shows salary (based on usual hours and usual wages) by gender
labour %>%
ggplot( aes(x = AGE_12, y = 0.01*HRLYEARN*UTOTHRS,color=GENDER)) +
  stat_summary(fun = mean, geom = "col", fill = "white") +
  labs(x = "AGE", y = "Mean Salary")


#hyst plot showing paid+unpaid hours for Females and Males during the week of sampling
labour %>%
  ggplot( aes(x = AGE_12, y = 0.01*(UNPAIDOT+PAIDOT),color=GENDER)) +
  stat_summary(fun = mean, geom = "col",fill=NA) +
  labs(x = "AGE", y = "Paid overtime hours")

# labour %>%
#   ggplot( aes(x = AGE_12, y = 0.01*HRLYEARN,color=GENDER)) +
#   stat_summary(fun = mean, geom = "col", fill = "white") +
#   labs(x = "AGE", y = "Mean Salary")


#idea: income=hours*rate for different ages, non-linear dependence vs linear regression

#LINEAR REGRESSION for SALARY vs TENURE
model<-lm(formula = 0.01*HRLYEARN*UTOTHRS~TENURE,data=labour)
model
labour %>% ggplot(aes(x=TENURE,y=0.01*HRLYEARN*UTOTHRS))+
  geom_point(color="orange")+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal()
#LINEAR REGRESSION for SALARY vs AGE (groups AGE_12 are treated as numbers)
#SECOND ORDER POLINOMIAL REGRESSION for SALARY vs AGE (AGE_12 is treated as numbers)

model<-lm(formula = 0.01*HRLYEARN*UTOTHRS~as.numeric(AGE_12),data=labour)
model
lm(formula = 0.01*HRLYEARN*UTOTHRS~poly(as.numeric(AGE_12),2),data=labour)

p1<-labour %>% ggplot(aes(x=as.numeric(AGE_12),y=0.01*HRLYEARN*UTOTHRS))+
  geom_point(color="orange")+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(method = "lm",formula = y ~poly(x,2), se = FALSE, color = "blue") +
  theme_minimal()

p2<-labour %>% ggplot(aes(x=AGE_12,y=0.01*HRLYEARN*UTOTHRS))+
  geom_boxplot(fill="orange")+
  theme_minimal()

p1/p2
#conclusion:
----------------------------------------------------------------------------------
#idea: check income of vulnerable stratas: women with kids/small kids, recent immigrants, low ed level
# for Canada and Ottawa only
  
  
# PART TIME vs GENDER

# reasons for part-time vs gender
tab<-xtabs(~ GENDER + WHYPT, data=labour )
tab
#Conclusion : women stay away from work taking care of the chieldren approx 10x then men

df_tab <- as.data.frame(tab)

df_tab %>%
 # filter(WHYPT %in% c("2","3","6")) %>%
  ggplot( aes(x = WHYPT , y=Freq, color=GENDER)) +
  geom_point(size=3)+
  theme(legend.position="top")+
  labs(x = "Why part-time job", 
       caption = "0	- Other reasons
1 -	Own illness or disability
2	- Caring for children
3	- Other personal or family responsibilities
4	- Going to school
5	- Personal preference
6	- Business conditions or could not find full-time work, looked for full-time work in last month
7	- Business conditions or could not find full-time work, did not look for full-time work in last month
", 
       y = "Number")+
  theme(
    plot.caption = element_text(hjust = 0, size = 10)
  )

# GENDER vs REASONS FOR LEAVING JOB
tab<-xtabs(~ GENDER + WHYLEFTN, data=labour )
tab
df_tab <- as.data.frame(tab)

df_tab %>%
  filter(WHYLEFTN %in% c("1","2","3","4","5")) %>%
  ggplot( aes(x = WHYLEFTN , y=Freq, color=GENDER)) +
  geom_point(size=3)+
  theme(legend.position="top")+
  labs(x = "Why left job in the last 12 months", 
       caption="
02	Job leavers, caring for children
03	Job leavers, pregnancy
04	Job leavers, personal or family responsibilities
05	Job leavers, going to school",
       y = "Number")+
theme(
  plot.caption = element_text(hjust = 0, size = 10)
)

# Conclusion: there are more males leaving work for school then females 
#
# part time going to school males 1664, females 2349
# left work because of the going to school males 810, females 738
# possible trend: males less likely work full or part-time during attending school
#compared to females
--------------------------------------------------------------------



#idea: make poisson bootstraps weights (or use binomial distribution instead of bootstrap)
# for Ottawa/ or Ontario only

# Confidence intervals via non-weighted bootstrap  
  
mean.function <- function(x, index) {
    d <- x[index]     # This first line will go in ever bootstrap function you make.
    return(mean(d,na.rm = TRUE))  
  }

BootHR<-boot(data = labour$HRLYEARN, statistic = mean.function, R=1000)
head(BootHR$t)
BootHR.graph <- data.frame(xbar=BootHR$t)

ggplot(BootHR.graph, aes(x=xbar)) +
  geom_histogram(color="darkblue", fill="lightblue") + 
  ggtitle('Estimated Sampling distribution of xbar' )

# 0.95 confidence interval
quantile( BootHR$t, probs=c(.025, .975) ) 
boot.ci(BootHR, type = "perc", conf = 0.95)  

#calling weighted bootstrap based on StatCAN code (separate file StatCa_poisson_weighted_bootstrap.R)
  
--------------------------------------------------------------
#idea make clasterization ? based on the age of children in family




#idea: some naive results: block-diagrams, clustering for income vs gender, income vs occupation industry
#in Canada and Ottawa

  
#idea: myth about low level of self impl/ rest buisness in Ottawa compared to Toronto



       
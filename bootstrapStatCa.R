#===============================================================================
# SAMPLE CODE FOR POISSON BOOTSTRAP ON THE LFS PUMF
# This program creates a file of 1,000 poisson bootstrap weights for use
# with the LFS PUMF, as well as shows a couple examples of how to
# calculate the variance of some estimates
# 
# Input_data = an LFS PUMF file already loaded into environment
# seed = number so that you can reproduce the random results
# Required packages:
#     dplyr
#     tidyr
#===============================================================================

#	Load required packages
library(dplyr)
library(tidyr)

#	Set seed and number of replicates here
seed <- 1234
reps <- 1000  # Number of bootstrap replicates. Use 10 for testing and learning purposes. Use 1000 for production of variance estimates.

#	Done in a series of functions, run the example under each one to follow along

#	As a pre-step for the calibration of bootstrap weights, define the
#	calibration age groups as defined in Appendix A of the LFS PUMF user guide
#	Temporarily map levels 1 and 2 of age_6 into levels 0 and 1 of age_12
#	(splitting age_12 15-19 age bracket into 15-16 and 17-19),
#	then convert to factor, merging levels into 10-year brackets for 35-44 and 45-54.

prep_data <- function(pumf) {
  pumf$age_cal <- factor(
    ifelse(
      pumf$age_6 %in% 1:2,
      as.numeric(pumf$age_6) - 1,
      as.numeric(pumf$age_12)
    ),
    levels = 0:12,
    labels = c(
      "15-16", "17-19", "20-24", "25-29", "30-34",
      rep("35-44", 2),
      rep("45-54", 2),
      "55-59", "60-64", "65-69", "70+"
    )
  )
  pumf
}

#	Prepare data
  # Input_data<-read.table(file='data/pub0325.txt',header=TRUE,sep="\t",stringsAsFactors = TRUE)
  # names(Input_data) <- tolower(names(Input_data))

Input_data<-labor
Input_data$age_12<-as.numeric(Input_data$age_12)
pumf <- prep_data(Input_data)
 
  
#	Function to create poisson factors for one replicate
sample_poisson_factors <- function(input) {
  sample(c(-1, 1), length(input), replace = TRUE)  # random and independent
}

#	Example to view poisson factors
#	List of 1 or -1 for each record and one replicate
poisson_factors <- sample_poisson_factors(pumf$finalwt)

# Labor Force Survey – User Guide
# Social Survey Methods Division

# Function to calculate the uncalibrated bootstrap weights 
# (using the sample_poisson_factors function above)
generate_replicates <- function(final_weight, n_reps, seed_value) {
  
  adjustment_factors <- final_weight * sqrt((final_weight - 1) / final_weight)  # equation (1)
  
  set.seed(seed_value)
  
  replicate(
    n_reps,
    final_weight + sample_poisson_factors(final_weight) * adjustment_factors,  # equation (1) + (2)
    simplify = "array"
  ) |> as.matrix()
}

# Example usage
uncal_bsw <- generate_replicates(pumf$finalwt, 10, seed)


# Function to calibrate each bootstrap replicate to the sums of final weights by domain
calibrate_weights <- function(uncalibrated_weights, final_weight, domains) {
  
  domain_indices <- split(seq_len(nrow(uncalibrated_weights)), domains)  # location on the pumf file where the groups are
  
  # Pull out all the ones in a group and sum the final weights
  domain_fw_totals <- domain_indices |>
    sapply(function(x) sum(final_weight[x]))
  
  # Pull out all the ones in a group and sum the bootstrap weights, for each replicate
  domain_bs_totals <- domain_indices |>
    sapply(function(x) colSums(uncalibrated_weights[x, ]))
  
  # Matrix transpose and calculate scaling factors
  domain_scaling_factors <- domain_fw_totals / t(domain_bs_totals)
  
  # Apply scaling factors by domain
  uncalibrated_weights * domain_scaling_factors[domains, ]
}

# Example usage
cal_bsw <- calibrate_weights(
  uncal_bsw, 
  pumf$finalwt, 
  interaction(pumf$prov, pumf$gender, pumf$age_cal)
)


# Final function that puts it all together
generate_bootstrap_weights <- function(d, n_reps, seed_value) {
  
  uncalibrated_weights <- generate_replicates(d$finalwt, n_reps, seed_value)
  domains <- interaction(d$prov, d$gender, d$age_cal)
  
  calibrate_weights(uncalibrated_weights, d$finalwt, domains)
}

# Example usage
final_bs <- pumf |> 
  dplyr::mutate(bswt = generate_bootstrap_weights(d = pumf, n_reps = reps, seed_value = seed))

#############################################################
# Example of using bootstrap weights to calculate variance
# Define indicators
final_bs$employed <- final_bs$lfsstat %in% c("Employed, at work","Employed, absent from work")
final_bs$unemployed <- final_bs$lfsstat %in% c("Unemployed")


#===============================================================================

#	Add NILF (Not in Labor Force) indicator
final_bs$nilf <- final_bs$lfsstat %in% c("Not in labour force")
#===============================================================================
# Estimates of totals
#===============================================================================
bs_total <- function(bootstrap_weights, final_weights) {
  est_fw <- sum(final_weights)
  est_bs <- colSums(bootstrap_weights)
  
  bs_var <- mean((est_bs - est_fw)^2)
  bs_sd  <- sqrt(bs_var)
  bs_cv  <- ifelse(est_fw != 0, abs(bs_sd / est_fw), 0)
  
  data.frame(
    est  = est_fw,
    var  = bs_var,
    sd   = bs_sd,
    cv   = bs_cv * 100,
    lb   = est_fw - qnorm(0.975) * bs_sd,
    ub   = est_fw + qnorm(0.975) * bs_sd,
    lbq  = quantile(est_bs, 0.025),
    ubq  = quantile(est_bs, 0.975),
    lbq2 = quantile(est_bs, 0.025, type = 2),
    ubq2 = quantile(est_bs, 0.975, type = 2)
  )
}

#===============================================================================
# Example: calculate total of unemployed by province using the above function
#===============================================================================
unemp_by_prov <- function(bw_file) {
  res <- bw_file |>
    dplyr::filter(unemployed) |>
    dplyr::group_by(prov) |>
    dplyr::summarize(
      est = bs_total(bswt, finalwt),
      .groups = "drop"
    ) |>
    tidyr::unpack(cols = est, names_sep = "_")
  
  res
}

#	Run example
results <- unemp_by_prov(final_bs)

#===============================================================================
# Estimates of ratios / proportions
#===============================================================================
bs_ratio <- function(bootstrap_weights, final_weights, num) {
  final_weights_num <- dplyr::case_when(num == TRUE ~ final_weights, TRUE ~ 0)
  est_fw_num <- sum(final_weights_num)
  est_fw_den <- sum(final_weights)
  est_fw     <- est_fw_num / est_fw_den
  
  bootstrap_weights_num <- dplyr::case_when(num == TRUE ~ bootstrap_weights, TRUE ~ 0)
  est_bs_num <- colSums(bootstrap_weights_num)
  est_bs_den <- colSums(bootstrap_weights)
  est_bs     <- est_bs_num / est_bs_den
  
  bs_var <- mean((est_bs - est_fw)^2)
  bs_sd  <- sqrt(bs_var)
  bs_cv  <- ifelse(est_fw != 0, abs(bs_sd / est_fw), 0)
  
  data.frame(
    est  = est_fw,
    var  = bs_var,
    sd   = bs_sd,
    cv   = bs_cv * 100,
    lb   = est_fw - qnorm(0.975) * bs_sd,
    ub   = est_fw + qnorm(0.975) * bs_sd,
    lbq  = quantile(est_bs, 0.025),
    ubq  = quantile(est_bs, 0.975)
  )
}
#===============================================================================
# Social Survey Methods Division
#===============================================================================

#	Function continued from previous block
#	Includes extended percentile intervals (type = 2)
bs_ratio <- function(bootstrap_weights, final_weights, num) {
  final_weights_num <- dplyr::case_when(num == TRUE ~ final_weights, TRUE ~ 0)
  est_fw_num <- sum(final_weights_num)
  est_fw_den <- sum(final_weights)
  est_fw     <- est_fw_num / est_fw_den
  
  bootstrap_weights_num <- dplyr::case_when(num == TRUE ~ bootstrap_weights, TRUE ~ 0)
  est_bs_num <- colSums(bootstrap_weights_num)
  est_bs_den <- colSums(bootstrap_weights)
  est_bs     <- est_bs_num / est_bs_den
  
  bs_var <- mean((est_bs - est_fw)^2)
  bs_sd  <- sqrt(bs_var)
  bs_cv  <- ifelse(est_fw != 0, abs(bs_sd / est_fw), 0)
  
  data.frame(
    est  = est_fw,
    var  = bs_var,
    sd   = bs_sd,
    cv   = bs_cv * 100,
    lb   = est_fw - qnorm(0.975) * bs_sd,
    ub   = est_fw + qnorm(0.975) * bs_sd,
    lbq  = quantile(est_bs, 0.025),
    ubq  = quantile(est_bs, 0.975),
    lbq2 = quantile(est_bs, 0.025, type = 2),
    ubq2 = quantile(est_bs, 0.975, type = 2)
  )
}

#===============================================================================
# Example: Unemployment rate by province using bs_ratio
#===============================================================================
unemprate_by_prov <- function(bw_file) {
  res <- bw_file |>
    dplyr::filter(!nilf) |>  # calculate unemployment rate on those in labour force
    dplyr::group_by(prov) |>
    dplyr::summarize(
      est = bs_ratio(bswt, finalwt, unemployed),
      .groups = "drop"
    ) |>
    tidyr::unpack(cols = est, names_sep = "_")
  
  res
}

#	Run example
results_ratio <- unemprate_by_prov(final_bs)

#===============================================================================
# My addition, Unemployment rate by gender using bs_ratio
#===============================================================================

unemprate_by_gender <- function(bw_file) {
  res <- bw_file |>
    dplyr::filter(!nilf) |>  # calculate unemployment rate on those in labour force
    dplyr::group_by(gender) |>
    dplyr::summarize(
      est = bs_ratio(bswt, finalwt, unemployed),
      .groups = "drop"
    ) |>
    tidyr::unpack(cols = est, names_sep = "_")
  
  res
}
#===============================================================================
# My addition, Unemployment rate by immigrant status using bs_ratio
#===============================================================================

unemprate_by_immig <- function(bw_file) {
  res <- bw_file |>
    dplyr::filter(!nilf) |>  # calculate unemployment rate on those in labour force
    dplyr::group_by(immig) |>
    dplyr::summarize(
      est = bs_ratio(bswt, finalwt, unemployed),
      .groups = "drop"
    ) |>
    tidyr::unpack(cols = est, names_sep = "_")
  
  res
}
 

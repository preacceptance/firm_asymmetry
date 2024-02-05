## Corporate Essence Analysis - Moral Asymmetry 


# Clear working directory
remove(list = ls())

# Set working directory to current file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get effect size
calculate_cohens_d <- function(t_value, df) {
  cohens_d <- 2 * t_value / sqrt(df)
  return(cohens_d)
}

# Example usage
t_value <- 2.17
df <- 138

calc_cohens_d <- calculate_cohens_d(t_value, df); calc_cohens_d

# get required sample
library(pwr)

calculate_sample_size <- function(cohens_d, power = 0.8, alpha = 0.05) {
  pwr_t_test <- pwr.t.test(
    d = cohens_d,             # Effect size (Cohen's d)
    power = power,            # Desired power (default: 0.8)
    sig.level = alpha,        # Significance level (default: 0.05)
    type = "paired"           # Paired samples t-test
  )
  
  return(ceiling(pwr_t_test$n))
}

# Example usage
cohens_d <- calc_cohens_d

required_sample_size <- calculate_sample_size(cohens_d); required_sample_size

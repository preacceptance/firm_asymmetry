## Corporate Essence Analysis - Moral Asymmetry 
## , 

# Clear working directory
remove(list = ls())

# Set working directory to current file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Get effect size for Newman et al. Study 2

calculate_effect_size <- function(f_value, df_between, df_within) {
  numerator <- (df_between * f_value)
  denominator <- (df_between * f_value) + df_within
  eta_squared <- numerator / denominator
  
  return(eta_squared)
}

f_value <- 6.35
df_between <- 3
df_within <- 153

calc_eff_size <- calculate_effect_size(f_value, df_between, df_within); calc_eff_size


#calculate needed sample

library(pwr)

calculate_sample_size <- function(effect_size, df_between, df_within, power = 0.9) {
  pwr_anova_test <- pwr.anova.test(
    k = df_between + 1,   # Number of groups (including control group)
    n = NULL,             # Sample size (to be determined)
    f = effect_size,      # Effect size
    sig.level = 0.05,     # Significance level (default: 0.05)
    power = power         # Desired power (default: 0.8)
  )
  
  return(ceiling(pwr_anova_test$n))
}

# Example usage
effect_size <- calc_eff_size
df_between <- 3
df_within <- 153


required_sample_size <- calculate_sample_size(effect_size, df_between, df_within); required_sample_size




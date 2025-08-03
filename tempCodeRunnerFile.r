# %% Calculate 99% confidence interval for Core Speed
x <- gpu_cleaned_df$Core_Speed

E <- function(x) {
  qnorm(p = 0.01/2, lower.tail = FALSE) * sd(x) / sqrt(length(x))
}
margin_error <- E(x)
lower_ci <- mean(x) - margin_error
upper_ci <- mean(x) + margin_error
mean_core_speed <- mean(x)

data.frame(
  Lower_CI_99 = lower_ci,
  Mean        = mean_core_speed,
  Upper_CI_99 = upper_ci
)
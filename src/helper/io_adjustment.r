get_prec_multipliers <- function(n, uncertainty)
{
  # uncertainty values
  # based on Renard 2010 WRR but modified
  MEAN_UNCERTAINTY <- 0.01
  mean_val <- rnorm(n, MEAN_UNCERTAINTY, 1 / (uncertainty^2))
  var_val <- LaplacesDemon::rinvchisq(n, uncertainty,
  scale <- abs(MEAN_UNCERTAINTY))
  sd_val <- sqrt(var_val)
  log_norm <- truncnorm::rtruncnorm(n, a = -0.4, b = 0.3,
                                    mean = mean_val, sd = sd_val)
  multiplier <- exp(log_norm) #0.67032-1.349859
  return(multiplier)
}



get_q_error <- function(q, standard_error = 0.1)
{
  # heterogeneity of variance
  # STANDARD_ERROR = 0.1 #used in Renard 2010

  mean_val <- 0
  sd_val <- (standard_error * q)
  error <- rnorm(length(sd_val), mean_val, sd_val)

  adjusted_error <- ifelse(q-error < 0, -q*(3*standard_error), error)
  return(adjusted_error)
}

# fit a Bayesian GP regression model to deaths and known cases data, integrating
# over uncertainty in the true CFR, and return the posterior mean and 95% CI
run_bayesian_model <- function (data, n_inducing = 10) {
  
  # only fit to time points where there are known cases
  data <- data %>%
    filter(cases_known > 0)
  
  library(greta.gp)
  
  n <- nrow(data)
  times <- seq(min(data$date_num), max(data$date_num))
  
  # GP parameters for squared-exponential kernel plus a bias term (intercept)
  lengthscale <- lognormal(4, 0.5)
  sigma <- normal(0, 1, truncation = c(0, Inf))
  temporal <- rbf(lengthscales = lengthscale,
                       variance = sigma ^ 2)
  intercept <- bias(1)
  kernel <- temporal + intercept
  
  # a set of inducing points at which to estimate the GPs; using a subset of
  # regressors approximation (put an inducing point at the last time, since we
  # care a lot about that estimate)
  inducing_points <- seq(min(times), max(times), length.out = n_inducing + 1)[-1]
  
  # GP for the (probit-) reporting rate
  z <- greta.gp::gp(times, kernel, tol = 1e-6)
  
  # convert to probabilities
  reporting_rate <- iprobit(z)
  
  # distribution over plausible baseline CFR values from China study. The 95%
  # CIs are symmetric around the estimate, so we assume it's an approximately
  # Gaussian distribution, truncated to allowable values.
  true_cfr_mean <- cCFRBaseline
  true_cfr_sigma <- mean(abs(cCFREstimateRange - cCFRBaseline)) / 1.96
  baseline_cfr_perc <- normal(true_cfr_mean, true_cfr_sigma, truncation = c(0, 100))
  
  # compute the expected number of deaths at each timepoint, given the true CFR,
  # number of reported cases with known outcomes, and reporting rate
  log_expected_deaths <-
    log(baseline_cfr_perc / 100) + log(data$cases_known) - log(reporting_rate) 
  expected_deaths <- exp(log_expected_deaths)
  
  # define sampling distribution
  distribution(data$deaths) <- poisson(expected_deaths)
  
  # construct the model
  m <- model(reporting_rate)
  
  # draw a bunch of mcmc samples (parallelising for multicore efficiency)
  country <- data$country[1]
  message("running model for ", country)
  draws <- mcmc(m, chains = 50, warmup = 500, n_samples = 1000, one_by_one = TRUE)
  
  # check convergence before continuing
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
  #n_eff <- coda::effectiveSize(draws)
  decent_samples <- all(r_hats$psrf[, 1] <= 1.2) 
  if (!decent_samples) 
  {
    draws <- extra_samples(draws, 2000, one_by_one = TRUE)
  }
  
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
  #n_eff <- coda::effectiveSize(draws)
  decent_samples <- all(r_hats$psrf[, 1] <= 1.2) 
  if (!decent_samples) 
  {
    draws <- extra_samples(draws, 2000,  one_by_one = TRUE)
  }
  
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
  #n_eff <- coda::effectiveSize(draws)
  decent_samples <- all(r_hats$psrf[, 1] <= 1.2) 
  if (!decent_samples) 
  {
    draws <- extra_samples(draws, 2000,  one_by_one = TRUE)
  }
  
  r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
  #n_eff <- coda::effectiveSize(draws)
  decent_samples <- all(r_hats$psrf[, 1] <= 1.2) 
  if (!decent_samples) 
  {
    draws <- extra_samples(draws, 2000,  one_by_one = TRUE)
  }
  
  # get estimates
  draws_mat <- as.matrix(draws)
  
  # compute posterior mean and 95% credible interval and return
  tibble::tibble(
    estimate = colMeans(draws_mat),
    lower = apply(draws_mat, 2, quantile, 0.025),
    upper = apply(draws_mat, 2, quantile, 0.975)
  )
  
}

# fit a Bayesian GP regression model to deaths and known cases data, integrating
# over uncertainty in the true CFR, and return the posterior mean and 95% CI
run_bayesian_model <- function (data, n_inducing = 5, verbose = TRUE) {
  
  # only fit to time points where there are known cases
  data <- data %>%
    filter(cases_known > 0)
  
  n <- nrow(data)
  times <- seq(min(data$date_num), max(data$date_num))
  
  # GP parameters for squared-exponential kernel plus a bias term (intercept)
  # for reporting rate
  lengthscale <- greta::lognormal(4, 0.5)
  sigma <- greta::lognormal(-1, 1)
  temporal <- greta.gp::rbf(lengthscales = lengthscale,
                            variance = sigma ^ 2)
  intercept <- bias(1)
  reporting_kernel <- temporal + intercept
  
  # IID noise kernel for observation overdispersion (clumped death reports)
  sigma_obs <- normal(0, 0.5, truncation = c(0, Inf))
  observation_kernel <- white(sigma_obs ^ 2)
  
  # combined kernel (marginalises a bunch of parameters for easier sampling)
  kernel <- reporting_kernel + observation_kernel
  
  # a set of inducing points at which to estimate the GPs; using a subset of
  # regressors approximation (put an inducing point at the last time, since we
  # care a lot about that estimate)
  inducing_points <- seq(min(times), max(times), length.out = n_inducing + 1)[-1]
  
  # GP for the (probit-) reporting rate
  z <- greta.gp::gp(times, inducing = inducing_points, kernel)
  
  # convert to probabilities
  reporting_rate <- greta::iprobit(z)
  
  # distribution over plausible baseline CFR values from China study. The 95%
  # CIs are symmetric around the estimate, so we assume it's an approximately
  # Gaussian distribution, truncated to allowable values.
  true_cfr_mean <- CFRBaseline
  true_cfr_sigma <- mean(abs(CFREstimateRange - CFRBaseline)) / 1.96
  baseline_cfr_perc <- greta::normal(true_cfr_mean, true_cfr_sigma, truncation = c(0, 100))
  
  # compute the expected number of deaths at each timepoint, given the true CFR,
  # number of reported cases with known outcomes, and reporting rate
  log_expected_deaths <-
    log(baseline_cfr_perc / 100) + log(data$cases_known) - log(reporting_rate) 
  expected_deaths <- exp(log_expected_deaths)
  
  # define sampling distribution
  greta::distribution(data$deaths) <- greta::poisson(expected_deaths)
  
  # construct the model
  m <- greta::model(reporting_rate)
  
  n_chains <- 50
  
  # sample initial values for hyperparameters from within their priors
  inits <- replicate(
    n_chains,
    initials(
      lengthscale = rlnorm(1, 4, 0.5),
      sigma = abs(rnorm(1, 0, 0.5)),
      sigma_obs = abs(rnorm(1, 0, 0.5)),
      baseline_cfr_perc = max(0.001, min(99.999,
        rnorm(1, true_cfr_mean, true_cfr_sigma)
      ))
    ),
    simplify = FALSE
  )
  
  if (verbose) {
    country <- data$country[1]
    message("running model for ", country)
  }
  
  # draw a bunch of mcmc samples
  draws <- greta::mcmc(
    m,
    sampler = hmc(Lmin = 15, Lmax = 20),
    chains = n_chains,
    warmup = 1000,
    n_samples = 1000,
    initial_values = inits,
    one_by_one = TRUE,
    verbose = verbose
  )
  
  # extend the number of chains until convergence (or give up)
  for (i in 1:5) {
    r_hats <- coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
    n_eff <- coda::effectiveSize(draws)
    decent_samples <- max(r_hats) <= 1.1 & min(n_eff) > 1000 
    if (!decent_samples) {
      if (verbose) {
        message("maximum R-hat: ", max(r_hats),
                "\nminimum n-eff: ", min(n_eff))
      }
      draws <- greta::extra_samples(draws, 2000, one_by_one = TRUE, verbose = verbose)
    }
  }
  
  # predict without IID noise (true reporting rate, without clumped death reporting)
  # could predict to more granular times here too
  z_smooth <- greta.gp::project(z, times, kernel = reporting_kernel)
  reporting_rate_smooth <- greta::iprobit(z_smooth)
  draws_pred <- greta::calculate(reporting_rate_smooth, values = draws)
  
  # get estimates
  draws_pred_mat <- as.matrix(draws_pred)
  
  # compute posterior mean and 95% credible interval and return
  tibble::tibble(
    estimate = colMeans(draws_pred_mat),
    lower = apply(draws_pred_mat, 2, quantile, 0.025),
    upper = apply(draws_pred_mat, 2, quantile, 0.975)
  )
  
}

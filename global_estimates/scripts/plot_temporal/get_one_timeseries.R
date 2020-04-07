# fit a Poisson GAM to deaths and known cases data, with a given value for the
# true CFR, and return the timeseries mean and 95% CI
get_one_timeseries <- function (true_cfr, data) {
  
  # fit model
  data <- data %>%
    mutate(log_offset = log(cases_known * true_cfr/100))
  
  model <- mgcv::gam(deaths ~ s(date_num, k = 3) + offset(log_offset),
                     data = data,
                     family = stats::poisson)
  
  # predict timeseries without log-offset to get timeseries -log(reporting rate)
  pred_data <- data.frame(date_num = data$date_num,
                          log_offset = 0)
  preds <- stats::predict(model,
                          newdata = pred_data,
                          type = "link",
                          se.fit = TRUE)
  
  # parameters of distribution over log reporting rate
  mu <- -preds$fit
  sigma <- preds$se.fit
  
  # convert to expectation and 95% CI over reporting rate and return
  tibble::tibble(
    estimate = exp(mu + (sigma ^ 2) / 2),
    lower = qlnorm(0.025, meanlog = mu, sdlog = sigma),
    upper = qlnorm(0.975, meanlog = mu, sdlog = sigma)
  )
  
}

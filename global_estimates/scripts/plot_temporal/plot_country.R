source('./scripts/plot_temporal/get_ci_poly.R')
source('./scripts/plot_temporal/cfr_plot_theme.R')

#plot time varying cfr for a country
plot_country <- function(plot_data){
  
  # modelB.P <- mgcv::gam(reporting_estimate ~ s(date_num) , data = plot_data, family = "gaussian")
  
  plot_data$log_offset <- log(plot_data$cases_known_adj)
  
  model <- mgcv::gam(deaths ~ s(date_num) + offset(log_offset), data = plot_data, family = "poisson")
  
  pred_data <- data.frame(date_num = plot_data$date_num,
                          log_offset = 0)
  preds <- stats::predict(model, newdata = pred_data, type = "link", se.fit = TRUE)
  
  # estimated reporting rate (expectation)
  mu <- -preds$fit
  sigma <- preds$se.fit
  estimate <- exp(mu + (sigma ^ 2) / 2)
  
  # 95% confidence interval
  ci_poly <- get_ci_poly(preds = preds, dates = plot_data$date)
  
  # clip all of these to (0, 1]
  estimate <- pmin(estimate, 1)
  ci_poly$y <- pmin(ci_poly$y, 1)
  
  
  p <- plot_data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_bw() + 
    ggplot2::geom_point(aes(x = date, y = reporting_estimate), size = 0.2) + 
    ggplot2::geom_path(aes(x = date, y = estimate), colour = 'red', size = 0.3) +
    ggplot2::geom_polygon(data = ci_poly, aes(x = x, y = y), fill = 'red', alpha = 0.2) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::ylab("Proportion of cases reported") +
    ggplot2::ggtitle(gsub("_", " ", plot_data$country %>% unique())) +
    cfr_plot_theme()
  
  return(p)
  
}
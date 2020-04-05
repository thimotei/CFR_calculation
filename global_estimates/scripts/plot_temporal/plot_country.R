source('./scripts/plot_temporal/run_bayesian_model.R')
source('./scripts/plot_temporal/cfr_plot_theme.R')

#plot time varying cfr for a country
plot_country <- function(plot_data){

  # get timeseries with the expectation, lower, and upper bounds of true CFR
  prediction <- run_bayesian_model(plot_data)

  estimate <- prediction$estimate
  ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                            y = c(prediction$upper, rev(prediction$lower)))

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
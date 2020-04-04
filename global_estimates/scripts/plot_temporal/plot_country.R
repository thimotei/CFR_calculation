source('./scripts/plot_temporal/get_ci_poly.R')
source('./scripts/plot_temporal/cfr_plot_theme.R')

#plot time varying cfr for a country
plot_country <- function(plot_data){
  
  modelB.P <- mgcv::gam(reporting_estimate ~ s(date_num) , data = plot_data, family = "gaussian")
  
  preds <- stats::predict(modelB.P, newdata = list(date_num = plot_data$date_num), type = "link", se.fit = TRUE)
  
  ci_poly <- get_ci_poly(preds = preds, dates = plot_data$date)
  
  p <- plot_data %>% 
    ggplot2::ggplot() +
    ggplot2::theme_bw() + 
    ggplot2::geom_point(aes(x = date, y = reporting_estimate), size = 0.2) + 
    ggplot2::geom_path(aes(x = date, y = preds$fit), colour = 'red', size = 0.3) +
    ggplot2::geom_polygon(data = ci_poly, aes(x = x, y = y), fill = 'red', alpha = 0.2) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::ylab("Proportion of cases reported") +
    ggplot2::ggtitle(gsub("_", " ", plot_data$country %>% unique())) +
    cfr_plot_theme()
  
  return(p)
  
}
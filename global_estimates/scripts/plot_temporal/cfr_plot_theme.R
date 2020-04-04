#default theme for plotting time varying cfr estimates
#can be changed to multiple themes for different outputs - html report / publication graphic
cfr_plot_theme <- function(){
  t <- ggplot2::theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 4),
             panel.grid = element_blank(),
             plot.title = element_text(hjust = 0.5, size = 7),
             text = element_text(size = 6))
  
  return(t)
}
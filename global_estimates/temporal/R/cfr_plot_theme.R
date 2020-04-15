#default theme for plotting time varying cfr estimates
#can be changed to multiple themes for different outputs - html report / publication graphic
cfr_plot_theme <- function(){
  t <- ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                      axis.text.x = ggplot2::element_text(size = 8),
                      axis.text.y = ggplot2::element_text(size = 8),
             panel.grid = ggplot2::element_blank(),
             plot.title = ggplot2::element_text(hjust = 0.5, size = 13),
             text = ggplot2::element_text(size = 6))
  
  return(t)
}
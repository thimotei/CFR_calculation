#get ggplot compatible polygon to plot  confidence interval
get_ci_poly <- function(preds, dates){
  
  mu <- -preds$fit
  sigma <- preds$se.fit
  
  lower <- qlnorm(0.025, meanlog = mu, sdlog = sigma)
  upper <- qlnorm(0.975, meanlog = mu, sdlog = sigma)
  
  poly_data <- tibble::tibble(x = c(dates, rev(dates)),
                              y = c(upper, rev(lower)))
  
  return(poly_data)
  
}
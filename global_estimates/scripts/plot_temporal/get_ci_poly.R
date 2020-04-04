#get ggplot compatible polygon to plot  confidence interval
get_ci_poly <- function(preds, dates){
  
  critval <- 1.96
  
  ci <- c(preds$fit + (critval * preds$se.fit), rev(preds$fit - (critval * preds$se.fit)))
  
  poly_data <- tibble::tibble(x = c(dates, rev(dates)), y = ci)
  
  return(poly_data)
  
}
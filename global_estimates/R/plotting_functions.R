# setting up colours to be used in the plots

col1 <- "#E69F00"
col2 <- "#56B4E9"
col3 <- "#009E73"
col4 <- "#F0E442"
col5 <- "#0072B2"
col6 <- "#D55E00"
col7 <- "#CC79A7"

# master function which when run makes Figure 1

master_plot <- function(data_in, 
                        startIntCFR,
                        startIntCD, 
                        legendPosition,
                        delay_dist,
                        delay_dist_truncated)
{

  dev.off()
  layout(matrix(c(1,
                  2,
                  3),
                nrow=3, byrow=TRUE))  
  
  par(oma = c(1,1,1,1) + 0.5,
      mar = c(0,0,1,1) + 0.1)

  plotDelays(delay_dist, delay_dist_truncated)

  subPlt2 <- plotCaseIncidence(data_in, startIntCD)
  subPlt3 <- plotDeathIncidence(data_in, startIntCD)
  
}


#plotting function to plot the delay distributions
plotDelays <- function(delay_dist,
                       delay_dist_truncated)
{
  
  par(mar=c(4,4,1,1),
      mgp=c(2,0.6,0))
  xSamplesDays <- seq(0,40, 1)
  xSamplesCurve <- seq(0,40, 0.1)
  samplingFromDelayDistDays <- delay_dist(xSamplesDays)
  samplingFromDelayDistCurve <- delay_dist(xSamplesCurve)
  plot(xSamplesDays, samplingFromDelayDistDays, 
       xlab = "Days after onset",
       pch = 19, 
       ylab = "P(death on a given day | death)",
       cex.lab = 1.8,
       cex.axis = 1.5)
  lines(xSamplesCurve, samplingFromDelayDistCurve, col = col1)

  xSamplesDays <- seq(0,40, 1)
  xSamplesCurve <- seq(0,40, 0.1)
  samplingFromDelayDistDays <- delay_dist_truncated(xSamplesDays)
  samplingFromDelayDistCurve <- delay_dist_truncated(xSamplesCurve)
  points(xSamplesDays, samplingFromDelayDistDays, 
       xlab = "Days after onset",
       pch = 19, 
       ylab = "",
       cex.lab = 1.8,
       cex.axis = 1.5)
  lines(xSamplesCurve, samplingFromDelayDistCurve, col = col2)
  mtext(LETTERS[1], adj = 0, line = 1, cex = 1.4) 
  legend("topright", legend=c("Non-truncated", "Truncated"),
         col=c(col1, col2), lty = 1:1, cex = 1.2)
  
}


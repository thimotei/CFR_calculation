col1 <- "#E69F00"
col2 <- "#56B4E9"
col3 <- "#009E73"
col4 <- "#F0E442"
col5 <- "#0072B2"
col6 <- "#D55E00"
col7 <- "#CC79A7"
  

master_plot <- function(data_in, 
                        startIntCFR,
                        startIntCD, 
                        legendPosition,
                        delay_dist)
{
  
  dev.off()
   #layout(matrix(c(1,
  #                 2,
  #                 3
  # ),
  # nrow=3, byrow=TRUE))
  
  par(mfrow =c(3,1),
      oma = c(1,1,1,1) + 0.5,
      mar = c(0,0,1,1) + 0.1)
   
  nCFRTimeSeriesOutput <- computenCFRTimeSeries(data_in, delay_dist)
  cCFRTimeSeriesOutput <- computecCFRTimeSeries(data_in, delay_dist)
  
  
  plotCFRTimeSeries(nCFRTimeSeriesOutput, cCFRTimeSeriesOutput, startIntCFR, legendPosition)
  plotCaseIncidence(data_in, startIntCD)
  plotDeathIncidence(data_in, startIntCD)
  
  
  
  outputEstimates <- cbind(tail(nCFRTimeSeriesOutput, n = 1), tail(cCFRTimeSeriesOutput, n = 1))
  return(outputEstimates)
  
  #subplot(plot1, plot2, plot3, nrows = 3, margin = 0.04)
  
  
}


plotCFRTimeSeries <- function(ncfr_data_in, 
                              ccfr_data_in, 
                              startIntCFR, 
                              legendPosition,
                              extraSubLabelSpace)
{

  endInt   <- length(ncfr_data_in$date)
  
  if(min(ncfr_data_in$ci_low, ccfr_data_in$ci_low)!= 0)
  {
    ylimMin <- min(ncfr_data_in$ci_low, ccfr_data_in$ci_low) 
  }
  else
  {
    ylimMin <- 0
  }
  
  ylimMax  <- max(ncfr_data_in$ci_high[startIntCFR:endInt], ccfr_data_in$ci_high[startIntCFR:endInt])
            + 0.1 * max(ncfr_data_in$ci_high[startIntCFR:endInt], ccfr_data_in$ci_high[startIntCFR:endInt])
  
  #par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  #par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
  par(xpd=FALSE)
    plotCI(x = ncfr_data_in$date[startIntCFR:endInt],
         y = ncfr_data_in$ci_mid[startIntCFR:endInt],
         li = ncfr_data_in$ci_low[startIntCFR:endInt],
         ui = ncfr_data_in$ci_high[startIntCFR:endInt],
         xlab = "", 
         ylab = "",
         ylim = c(ylimMin,ylimMax),
         cex.lab = 1.4,
         cex.axis = 1.4)
  lines(x = ncfr_data_in$date[startIntCFR:endInt], 
        ncfr_data_in$ci_mid[startIntCFR:endInt],
        col = col2)
  grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
  par(xpd=TRUE)
  #text(ncfr_data_in$date[startInt] + 0.6 , 3.8, "A", cex = 1.5)
  
  par(new=T)
  
  par(xpd=FALSE)
  #par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  #par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  par(mar = c(3,4,1,1),mgp = c(2,0.6,0))
  plotCI(x = ccfr_data_in$date[startIntCFR:endInt],
         y = ccfr_data_in$ci_mid[startIntCFR:endInt],
         li = ccfr_data_in$ci_low[startIntCFR:endInt],
         ui = ccfr_data_in$ci_high[startIntCFR:endInt], 
         xlab = "",
         ylab = "CFR (%)",
         ylim = c(ylimMin,ylimMax),
         cex.lab = 1.4,
         cex.axis = 1.4)
  lines(x = ccfr_data_in$date[startIntCFR:endInt], 
        ccfr_data_in$ci_mid[startIntCFR:endInt], col = col3)
  legend(legendPosition, legend=c("naive CFR", "corrected CFR"),
         col=c(col2, col3), lty = 1:1, cex = 1.2)
  mtext(LETTERS[1], adj = 0, line = 1) 
}


plotCaseIncidence <- function(data_in_raw, startIntCD)
{
  endInt <- length(data_in_raw$date)
  par(xpd=FALSE)
  plot(x = data_in_raw$date[startIntCD:endInt],
       data_in_raw$new_cases[startIntCD:endInt],
       xlab = "",
       ylab = "Incidence of confirmed cases",
       cex.lab = 1.4,
       cex.axis = 1.4)
  lines(x = data_in_raw$date[startIntCD:endInt], data_in_raw$new_cases[startIntCD:endInt], col = col5)
  par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
  #par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  #par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
  par(xpd=TRUE)
  mtext(LETTERS[2], adj = 0, line = 1) 
  #text(data_in_raw$date[1] + 1 , 115, "C", cex = 1.5)
  
}

plotDeathIncidence <- function(data_in_raw, startIntCD)
{
  endInt <- length(data_in_raw$date)
  par(xpd=FALSE)
  plot(x = data_in_raw$date[startIntCD:endInt], 
       data_in_raw$new_deaths[startIntCD:endInt],
       xlab = "Date",
       ylab = "Incidence of deaths",
       cex.lab = 1.4,
       cex.axis = 1.4)
  lines(x = data_in_raw$date[startIntCD:endInt], data_in_raw$new_deaths[startIntCD:endInt], col = col6)
  par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
  #par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
  #par(mar = c(2, 2, 1, 1)) # make the plots be closer together
  grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
  par(xpd=TRUE)
  mtext(LETTERS[3], adj = 0, line = 1) 
  #text(data_in_raw$date[1] + 1 , 1.95, "D", cex = 1.5)
  
}


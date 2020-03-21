# main loop preparing data and calculating CFR
outputcCFR <- NA
outputcCFRTotal <- NA
for(i in 1:length(countries))
{
  tryCatch({
    data  <- allDatDesc %>% dplyr::filter(country == countries[i])
    data2 <- data %>% dplyr::filter(., any(new_deaths != 0))
    if(dim(data2)[1] != 0)
    {
      data3 <- data2 %>% pad
      data4 <- data3 %>% mutate(country = replace_na(country, head(country, n = 1)),
                                new_cases  = replace_na(new_cases, 0),
                                new_deaths = replace_na(new_deaths, 0))
      
      data5 <- data4 %>% as.data.frame
      outputcCFR <- cCFR_with_CIs_main_time_series(data4, hospitalisation_to_death_truncated)
      outputcCFR$country <- data4$country[1]
    }
    else
    {
      next
    }
  },
  error=function(e){cat("ERROR: skipping country, insufficient data", "\n")})
  outputcCFRTotal <- rbind(outputcCFRTotal, outputcCFR)
}

# loop building readable table of estimates
countriescCFR <- unique(outputcCFRTotal$country)
currentEstimatescCFR <- NA
for(i in 1:length(countriescCFR))
{
  data  <- outputcCFRTotal %>% dplyr::filter(country == countriesCFR[i])
  data2 <- data %>% dplyr::filter(date == max(date))
  currentEstimatescCFR <- distinct(drop_na(as_tibble(rbind(currentEstimatescCFR, data2))))
}
 
# cleaning data and putting into form for the large table of results
currentEstimatescCFR <- mutate(currentEstimatescCFR, ci_mid = ci_mid %>% signif(., 2),
                               ci_low = ci_low %>% signif(., 2),
                               ci_high = ci_high %>% signif(., 2))
currentEstimatescCFR <- dplyr::rename(currentEstimatescCFR, 
                                      Date = date,
                                      cCFR = ci_mid,
                                      Low.CI = ci_low, 
                                      High.CI = ci_high,
                                      Country = country)
currentEstimatescCFR <- currentEstimatescCFR %>% select(Date, Country, cCFR, Low.CI, High.CI)
currentEstimatescCFR <- mutate(currentEstimatescCFR,
                               cCFR = cCFR %>% signif(2),
                               Low.CI = Low.CI %>% signif(2),
                               High.CI = High.CI %>% signif(2))
currentEstimatescCFR <- subset(currentEstimatescCFR, Country != "Cases_on_an_international_conveyance_Japan")
currentEstimatescCFRNeat <- paste(currentEstimatescCFR$cCFR,"% ("
                            ,currentEstimatescCFR$Low.CI,"-"
                            ,currentEstimatescCFR$High.CI,")",sep="")
currentEstimatescCFRNeatDF <- data.frame(Date = currentEstimatescCFR$Date,
                                   Country = currentEstimatescCFR$Country,
                                   cCFREstimates = currentEstimatescCFRNeat)

# calculating the underporting level and getting it into format for big results table
underreportingDF <- currentEstimatescCFR %>% mutate(Date,
                                Country, 
                                underreporting_estimate = signif(1/cCFR*100, 2),
                                underreporting_estimate_low_CI = signif(1/High.CI*100, 2),
                                underreporting_estimate_high_CI = signif(1/Low.CI*100, 2))

underreportingDF$underreporting_estimate[underreportingDF$underreporting_estimate >= 100] <- 100
underreportingDF$underreporting_estimate_high_CI[underreportingDF$underreporting_estimate_high_CI >= 100] <- 100
underreportingDF <-  underreportingDF %>% select(Date, Country, underreporting_estimate, underreporting_estimate_low_CI, underreporting_estimate_high_CI)

underreportingDF <- subset(underreportingDF, Country != "Cases_on_an_international_conveyance_Japan")

underreportingDF$Country <- underreportingDF$Country %>% stringr::str_replace_all("_", " ") 
underreportingDF$Country <- underreportingDF$Country %>% stringr::str_replace_all("CANADA", "Canada") 

underReportingNeat <- paste(underreportingDF$underreporting_estimate,"% (",underreportingDF$underreporting_estimate_low_CI,"-",underreportingDF$underreporting_estimate_high_CI,")",sep="")
underReportingNeatDF <- data.frame(Date = underreportingDF$Date,
                                  Country = underreportingDF$Country,
                                  UnderreportingEstimate = underReportingNeat)

# exporting the various data files for the table and the plot
saveRDS(underreportingDF, file = "data/current_underreporting_estimates_broken.rds")
saveRDS(underReportingNeatDF, file = "data/current_underreporting_estimates_neat.rds")
saveRDS(currentEstimatescCFRNeatDF, file = "data/current_ccfr_estimates.rds")

allResults <- data.frame(Country = underreportingDF$Country,
                         UnderreportingEstimate = underReportingNeat,
                         currentEstimatescCFRNeatDF$cCFREstimates,
                         currentEstimatesnCFRNeatDF$nCFREstimates)

# used for the plot
saveRDS(allResults, file = "data/all_results.rds")

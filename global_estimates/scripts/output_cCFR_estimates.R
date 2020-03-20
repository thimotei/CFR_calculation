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

countriescCFR <- unique(outputcCFRTotal$country)

currentEstimatescCFR <- NA
for(i in 1:length(countriescCFR))
{
  data  <- outputcCFRTotal %>% dplyr::filter(country == countriesCFR[i])
  data2 <- data %>% dplyr::filter(date == max(date))
  currentEstimatescCFR <- distinct(drop_na(as_tibble(rbind(currentEstimatescCFR, data2))))
}
 

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
                               cCFR = cCFR %>% signif(cCFR, 2),
                               Low.CI = Low.CI %>% signif(Low.CI, 2),
                               High.CI = High.CI %>% signif(High.CI, 2))

currentEstimatescCFR <- subset(currentEstimatescCFR, Country != "Cases_on_an_international_conveyance_Japan")

underreportingDF <- currentEstimatescCFR %>% mutate(Date,
                                Country, 
                                underreporting_estimate = signif(1/cCFR*100, 2),
                                underreporting_estimate_low_CI = signif(1/High.CI*100, 2),
                                underreporting_estimate_high_CI = signif(1/Low.CI*100, 2))

underreportingDF$underreporting_estimate_high_CI[underreportingDF$underreporting_estimate_high_CI >= 100] <- 100
underreportingDF <-  underreportingDF %>% select(Date, Country, underreporting_estimate, underreporting_estimate_low_CI, underreporting_estimate_high_CI)

underreportingDF <- subset(underreportingDF, Country != "Cases_on_an_international_conveyance_Japan")

saveRDS(underreportingDF, file = "data/current_underreporting_estimates.rds")
saveRDS(currentEstimatescCFR, file = "data/current_ccfr_estimates.rds")

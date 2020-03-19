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
                                      nCFR = ci_mid,
                                      Low.CI = ci_low, 
                                      High.CI = ci_high,
                                      Country = country)

currentEstimatescCFR <- currentEstimatescCFR %>% select(Date, Country, nCFR, Low.CI, High.CI)

saveRDS(currentEstimatescCFR, file = "data/current_ccfr_estimates.rds")

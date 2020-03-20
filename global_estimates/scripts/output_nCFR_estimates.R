outputnCFRTotal <- NA
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
      outputnCFR <- nCFR_with_CIs_main_time_series(data5, hospitalisation_to_death_truncated)
      outputnCFR$country <- data4$country[1]
      
    }
    else
    {
      next
    }
    #print(data4)
    #print(i)
  },
  error=function(e){cat("ERROR: skipping country, insufficient data", "\n")})
  outputnCFRTotal <- rbind(outputnCFRTotal, outputnCFR)
}

countriesCFR <- unique(outputnCFRTotal$country)

currentEstimatesnCFR <- NA
for(i in 1:length(countriesCFR))
{
  data  <- outputnCFRTotal %>% dplyr::filter(country == countriesCFR[i])
  data2 <- data %>% dplyr::filter(date == max(date))
  currentEstimatesnCFR <- distinct(drop_na(as_tibble(rbind(currentEstimatesnCFR, data2))))
}



currentEstimatesnCFR <- dplyr::rename(currentEstimatesnCFR, 
                                      Date = date,
                                      nCFR = ci_mid,
                                      Low.CI = ci_low, 
                                      High.CI = ci_high,
                                      Country = country)

currentEstimatesnCFR <- currentEstimatesnCFR %>% select(Date, Country, nCFR, Low.CI, High.CI)

currentEstimatesnCFR <- mutate(currentEstimatesnCFR,
                               nCFR = nCFR %>% signif(., 2),
                               Low.CI = Low.CI %>% signif(., 2),
                               High.CI = High.CI %>% signif(., 2))

currentEstimatesnCFR <- subset(currentEstimatesnCFR, Country != "Cases_on_an_international_conveyance_Japan")

saveRDS(currentEstimatesnCFR, file = "data/current_ncfr_estimates.rds")

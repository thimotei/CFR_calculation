# Reads and cleans data from EDCD website using NCoVUtils package by Funk et al.
allDat <- NCoVUtils::get_ecdc_cases()
allDatDesc <- allDat %>% dplyr::arrange(country, date)
allDatDesc <- allDatDesc %>% dplyr::mutate(date = lubridate::ymd(date)) %>% dplyr::rename(new_cases = cases,
                                                                                          new_deaths = deaths)
                            
allDatDesc <- allDatDesc %>% dplyr::select(date, country, new_cases, new_deaths)
allDatDesc$date <- as.Date(allDatDesc$date)
countries <- allDatDesc$country %>% unique

# outputs total deaths for the summary plot, put together in the markdown file
totalCasesDF <- allDatDesc %>% group_by(country) %>% summarise(total_cases = sum(new_cases))
totalDeathsDF <- allDatDesc %>% group_by(country) %>% summarise(total_deaths = sum(new_deaths))
totalDeathsDF$country <- totalDeathsDF$country %>% stringr::str_replace_all("_", " ")

# total deaths used solely for the plot
saveRDS(totalDeathsDF, file = "data/total_deaths.rds")


# calculating quantiles for the summary plot 
quantilesRes <- c()
for(i in 1:length(underreportingDF$underreporting_estimate_low_CI))
{
  tmp <- c(underreportingDF$underreporting_estimate_low_CI[i],underreportingDF$underreporting_estimate_high_CI[i])
  quantilesRes<- rbind(quantilesRes,quantile(tmp, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
}


# putting everything needed for the plot together in one dataframe and exporting it
allTogetherPlotDF <- data.frame(country = underreportingDF$Country,
                                underreporting_estimate = underreportingDF$underreporting_estimate,
                                lowCI = underreportingDF$underreporting_estimate_low_CI,
                                highCI = underreportingDF$underreporting_estimate_high_CI,
                                quantile2.5 = quantilesRes[,1],
                                quantile25 = quantilesRes[,2],
                                quantile50 = quantilesRes[,3],
                                quantile75 = quantilesRes[,4],
                                quantile95.5 = quantilesRes[,5])

saveRDS(allTogetherPlotDF, file = "data/all_together_plot.rds")

casesDeathsDF <- dplyr::left_join(totalCasesDF, totalDeathsDF)
casesDeathsDF$country <- casesDeathsDF$country %>% stringr::str_replace_all("_", " ") 
all_results <- dplyr::left_join(underReportingNeatDF, casesDeathsDF) %>% select(-Date)

saveRDS(all_results, "data/all_results_reporting_cases_deaths")

# read in HPC output data neatly

allHPCBayesianData <- function()
{
  httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
  allDatRaw <- readr::read_csv(tf) %>%
    dplyr::rename(date = dateRep, 
                  country = countriesAndTerritories,
                  countryCode = countryterritoryCode) %>%
    dplyr::mutate(date = lubridate::dmy(date))
  
  countryCodesLookUp <- allDatRaw %>%
    dplyr::select(country, 
                  countryCode) %>% 
    unique()
  
  deathSummaryData <- allDatRaw %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalDeaths = sum(deaths)) %>% 
    dplyr::mutate(country_order = rank(totalDeaths)) %>%
    dplyr::arrange(desc(country_order))

  caseSummaryData <- allDatRaw %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(totalCases = sum(cases))
  
  
  data_path <- "~/Dropbox/bayesian_underreporting_estimates/temp/"
  files <- dir(path = data_path,
               pattern = "*.rds")
  
  dataTmp <- dplyr::tibble(countryCode = files) %>% 
    dplyr::mutate(file_contents = purrr::map(countryCode, 
                                             ~ readRDS(file.path(data_path, .)))
                  
    ) %>% 
    tidyr::unnest(cols = c(file_contents)) %>%
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, "result_")) %>% 
    dplyr::mutate(countryCode = stringr::str_remove(countryCode, ".rds")) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::mutate(date = seq(Sys.Date() - 13 - dplyr::n()  + 1, Sys.Date() - 13, 1)) %>% 
    dplyr::select(date, everything()) %>%
    dplyr::left_join(countryCodesLookUp) %>%
    dplyr::select(date, country, countryCode, everything()) %>%
    dplyr::group_by(countryCode) %>%
    dplyr::ungroup()
  
  data <- dataTmp %>% 
    dplyr::left_join(deathSummaryData, by = c('country' = 'country')) %>% 
    dplyr::left_join(caseSummaryData, by = c('country' = 'country')) %>% 
    dplyr::arrange(desc(country_order), date) %>%
    dplyr::mutate(country = stringr::str_replace_all(country, "_", " "))
  
  return(data)
  
}


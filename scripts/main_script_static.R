# Code to calculate estimates for under-reporting as of today
# Author: Tim Russell

# Set paths
setwd(here::here())

# Source functions 
source("global_estimates/R/scale_CFR.R")
source("global_estimates/R/delay_distributions.R")
source("global_estimates/R/table_of_estimates.R")

# setting the baseline CFR
cCFRBaseline <- 1.4
cCFREstimateRange <- c(1.2, 1.7)

# set parameters of delay distribution -----------------------------------

# lower end of the range
zmeanLow <- 8.7
zmedianLow <- 6.7
muLow <- muTransform(zmedianLow)
sigmaLow <- sigmaTransform(zmeanLow, muLow)


# middle of the range
zmeanMid <- 13
zmedianMid <- 9.1
muMid <- muTransform(zmedianMid)
sigmaMid <- sigmaTransform(zmeanMid, muMid)

# upper end of the range
zmeanHigh <- 20.9
zmedianHigh <- 13.7
muHigh <- muTransform(zmedianHigh)
sigmaHigh <- sigmaTransform(zmeanHigh, muHigh)

# Load data -----------------------------------------------------

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read.csv(tf)


# munge data, pad data and select only those with greater than 10 deaths
allTogetherClean <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(country != "CANADA", 
                country != "Cases_on_an_international_conveyance_Japan") %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)

# calculate table of estimates using three delay distributions (both ends of the reported ranges and the mean)
allTogetherLow <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_low) 
allTogetherMid <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_mid) 
allTogetherHigh <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_high)


# choosing CIs such that they include all uncertainty from delay distribution
finalRes <- dplyr::tibble(
  country = allTogetherMid$country,
  total_cases = allTogetherMid$total_cases,
  total_deaths = allTogetherMid$total_deaths,
  underreporting_estimate  = pmin(allTogetherLow$underreporting_estimate, allTogetherMid$underreporting_estimate, allTogetherHigh$underreporting_estimate),
  lower = pmin(allTogetherLow$lower, allTogetherMid$lower, allTogetherHigh$lower),
  upper = pmax(allTogetherLow$upper, allTogetherMid$upper, allTogetherHigh$upper))


# putting all of the data together in a readable format for the Rmd file
reportDataFinal <- finalRes %>%
  dplyr::select(country, total_cases, total_deaths, underreporting_estimate, lower,
                upper) %>%
  dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
  dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
  dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
  dplyr::mutate(lower = signif(lower, 2)) %>%
  dplyr::mutate(upper = signif(upper, 2)) %>%
  dplyr::ungroup(country) %>%
  dplyr::mutate(country = country %>% stringr::str_replace_all("_", " ")) %>% 
  dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                       "% (",lower*100,"% - ",upper*100,"%)"))

# saving the output
saveRDS(reportDataFinal, "global_estimates/data/reportDataFinal.rds")

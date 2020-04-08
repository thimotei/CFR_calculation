# Code to fit GAMs to time-series of under-reporting estimates

# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(mgcv)
require(gridExtra)
require(ggplot2)

# Set paths
setwd(here::here())

#source data processing and plotting scripts
source("global_estimates/temporal/R/cfr_plot_theme.R")
source("global_estimates/temporal/R/get_one_timeseries.R")
source("global_estimates/temporal/R/get_plot_data.R")
source("global_estimates/temporal/R/plot_country.R")
source("global_estimates/temporal/R/scale_CFR_temporal.R")


# Set parameters
zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
#cCFRIQRRange <- c(1.3, 1.4)


# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, muHDT, sigmaHDT) - plnorm(x, muHDT, sigmaHDT)
}


# Load data -----------------------------------------------------
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read_csv(tf)


allDatDesc <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan"))

# Do analysis
allTogetherCleanA <- allDatDesc %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  #What is this doing?
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)


# Plot rough reporting over time -----------------------------------------

plot_country_names <- allTogetherCleanA %>% 
  dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  dplyr::filter(death_cum_sum >= 10) %>% 
  dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
  dplyr::arrange(-max_deaths) %>% 
  dplyr::group_by(country) %>% 
  dplyr::filter(n() >= 8) %>%
  dplyr::pull(country) %>% 
  unique()


cfr_plots <- list()
for (country_name in plot_country_names){
  plot_data <- get_plot_data(country_name = country_name)
  
  p <- try(plot_country(plot_data = plot_data))
  
  if ('try-error' %in% class(p)){next}
  
  cfr_plots[[country_name]] = p
  
}

cfr_plot_grid = arrangeGrob(grobs = cfr_plots,
                            ncol = 4,
                            left = "Percentage of symptomatic cases reported", 
                            rot = 90)


ggsave('global_estimates/outputs/figure_1.png',
       cfr_plot_grid,
       width =11, 
       height = 25, 
       units = 'in', 
       dpi = 450)


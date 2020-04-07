# Code to estimate reporting

# Sanbox to look at temporal variation in reporting


# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(mgcv)
require(gridExtra)
require(ggplot2)

# Set paths
setwd("~/Documents/lshtm/github repos/CFR_calculation/global_estimates/")
if(grepl(Sys.info()["user"], pattern = "^adamkuchars(ki)?$")){setwd("~/Documents/GitHub/CFR_calculation/global_estimates/")}

#source data processing and plotting scripts
source('./scripts/plot_temporal/get_plot_data.R')
source('./scripts/plot_temporal/plot_country.R')

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


# Define CFR function -----------------------------------------------------

# Function to work out correction CFR
scale_cfr_temporal <- function(data_1_in, delay_fun = hospitalisation_to_death_truncated){

  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  cumulative_known_t <- NULL # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- c(cumulative_known_t,known_i) # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- (death_incidence/cumulative_known_t) %>% pmin(.,1)
  
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
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

cfr_plot_grid = arrangeGrob(grobs = cfr_plots, ncol = 4)

# ggsave('outputs/cfr_plot_grid.pdf',
#        cfr_plot_grid,
#        width = 6, 
#        height = 18, 
#        units = 'in', 
#        useDingbats = FALSE,
#        dpi = 400)

ggsave('outputs/figure_1.png',
       cfr_plot_grid,
       width =6, 
       height = 18, 
       units = 'in', 
       dpi = 400)


 
  

# Code to estimate reporting

# Sanbox to look at temporal variation in reporting


# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)

# Set paths
setwd("~/Documents/lshtm/github repos/CFR_calculation/global_estimates/")
if(grepl(Sys.info()["user"], pattern = "^adamkuchars(ki)?$")){setwd("~/Documents/GitHub/CFR_calculation/global_estimates/")}

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
allDat <- read.csv(tf)


allDatDesc <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(country != "CANADA", 
                country != "Cases_on_an_international_conveyance_Japan")
# Do analysis
allTogetherCleanA <- allDatDesc %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths) 



# Plot rough reporting over time -----------------------------------------

countrypick <- "Denmark"
#countrypick <- "United_Kingdom"
#countrypick <- "Germany"

data_1_in_Pick <- allTogetherCleanA %>% filter(country==countrypick)
true_cfr <- 1.4/100

out_cfr <- scale_cfr_temporal(data_1_in_Pick)

# Rough plot
par(mar=c(3,3,1,1),mgp=c(2,0.6,0))

plot(data_1_in_Pick$date-zmeanHDT,true_cfr/out_cfr$cCFR,ylim=c(0,1),ylab="reported",xlab="",main=countrypick)


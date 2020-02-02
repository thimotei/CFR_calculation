
library(tidyverse)

# - - -
# Set user-specific directory path and load datasets
if(Sys.info()["user"]=="adamkuchars" | Sys.info()["user"]=="adamkucharski") {
  setwd("~/Documents/GitHub/CFR_calculation/")
}



# Load data and functions ---------------------------------------------------------------

source("R/functions.R")

# Load case & death timeseries
all_dat <- read_csv("data/case_death_data_WHO_extrapolated.csv")
all_dat <- all_dat %>% mutate(naive_cfr = cumulative_deaths/cumulative_cases) # Add naive cfr

scaled_reporting <- 65 # scaling factor (from Kucharski et al 2020)
cfr_from_date <- as.Date("2020-01-26") # set date to calculate CFR from
cfr_current_date <- as.Date("2020-02-02") # set current date
cfr_max_date <- max(all_dat$date)

# Define onset-to-death distribution
# 9.1 day onset-to-hospitalisation in Li et al NEJM paper

meanG <- 13.8
scaleG <- 1
onset_to_death <- function(x){ dgamma(x, meanG/scaleG, scale = scaleG) }

meanH <- 9.1
scaleH <- 1
onset_to_hosp <- function(x){ dgamma(x, meanH/scaleH, scale = scaleH) }


# Plot data ---------------------------------------------------------------

plot_cfr_basic()




library(tidyverse)

# - - -
# Set user-specific directory path and load datasets
if(Sys.info()["user"]=="adamkuchars" | Sys.info()["user"]=="adamkucharski") {
  setwd("~/Documents/GitHub/CFR_calculation/")
}



# Load data and functions ---------------------------------------------------------------

source("R/functions.R")
source("R/format_data.R")

# Define onset-to-death distribution
# 9.1 day onset-to-hospitalisation in Li et al NEJM paper

# NOTE: Currently looking at date of confirmation. So actual delay may be lower?

meanG <- 13.8 # subtract delay from onset-to-confirmation
scaleG <- 1
onset_to_death <- function(x){ dgamma(x, meanG/scaleG, scale = scaleG) }

meanH <- 9.1
scaleH <- 1
onset_to_hosp <- function(x){ dgamma(x, meanH/scaleH, scale = scaleH) }

# Period to use for extrapolation (for convergence calculation only NOT forecast)
extrapolate_use_days <- 10 # Use 7 day growth trend to extrapolate from
extrapolate_length <- 0 # days to extrapolate

scaled_reporting <- 1 # scaling factor (from Kucharski et al 2020)
cfr_from_date <- as.Date("2020-01-26") # set date to show CFR from
cfr_current_date <- max(all_dat$date) # set max date in data
cfr_max_date <- max(all_dat$date) + extrapolate_length

#source("R/extrapolate_data.R")

# Plot data ---------------------------------------------------------------

plot_cfr_basic(all_dat)



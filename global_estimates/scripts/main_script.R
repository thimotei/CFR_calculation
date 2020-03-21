library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(padr)
library(tidyverse)

# DEPRACATED, used in early analysis. When equal to one, it doesn't do anything

scaled_reporting <- 1

######### parameterising the delay distributions (taken from papers) ############

# The distribution of time from hospitalisation-to-death 
# taken from Linton et al. (https://doi.org/10.3390/jcm9020538)

zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))

hospitalisation_to_death_truncated <- function(x)
{
  dlnorm(x, muHDT, sigmaHDT)
}

source("scripts/read_data.R")
source("R/cCFR_calculation.R")
source("scripts/output_cCFR_estimates.R")
source("scripts/output_nCFR_estimates.R")
source("R/make_plot.R")

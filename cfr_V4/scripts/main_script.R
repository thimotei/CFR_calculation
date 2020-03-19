library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(padr)

scaled_reporting <- 1

######### parameterising the delay distributions (taken from papers) ############

# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)

zmeanOD <- 14.5
zsdOD <- 6.7
zmedianOD <- 13.2
muOD <- log(zmedianOD)
sigmaOD <- sqrt(2*(log(zmeanOD) - muOD))

onset_to_death <- function(x)
{
  dlnorm(x, muOD, sigmaOD)
}

zmeanODT <- 20.2
zsdODT <- 11.6
zmedianODT <- 17.1
muODT <- log(zmedianODT)
sigmaODT <- sqrt( 2*(log(zmeanODT) - muODT))

onset_to_death_truncated <- function(x)
{
  dlnorm(x, muODT, sigmaODT)
}

zmeanHD <- 8.6
zsdHD <- 6.7
zmedianHD <- 6.7
muHD <- log(zmedianHD)
sigmaHD <- sqrt(2*(log(zmeanHD) - muHD))

hospitalisation_to_death <- function(x)
{
  dlnorm(x, muHD, sigmaHD)
}

zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))

hospitalisation_to_death_truncated <- function(x)
{
  dlnorm(x, muHDT, sigmaHDT)
}

#source("R/read_data.R")
source("R/cCFR_calculation.R")
source("R/plotting_functions.R")

library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(padr)

# read in data

scaled_reporting <- 1.0

source("R/read_data.R")

######### parameterising the delay distributions (taken from papers) ############



# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)


zmean <- 14.5
zsd <- 6.7
zmedian <- 13.2
muOD <- log(zmedian)
sigmaOD <- sqrt( 2*(log(zmean) - muOD) )

onset_to_death <- function(x)
{
  dlnorm(x, muOD, sigmaOD)
}

zmean <- 20.2
zsd <- 11.6
zmedian <- 17.1
muODT <- log(zmedian)
sigmaODT <- sqrt( 2*(log(zmean) - muODT) )

onset_to_death_truncated <- function(x)
{
  dlnorm(x, muODT, sigmaODT)
}

zmean <- 8.6
zsd <- 6.7
zmedian <- 6.7
muHD <- log(zmedian)
sigmaHD <- sqrt( 2*(log(zmean) - muHD) )

hospitalisation_to_death <- function(x)
{
  dlnorm(x, muHD, sigmaHD)
}

zmean <- 13
zsd <- 12.7
zmedian <- 9.1
muHDT <- log(zmedian)
sigmaHDT <- sqrt( 2*(log(zmean) - muHDT) )

hospitalisation_to_death_truncated <- function(x)
{
  dlnorm(x, muHDT, sigmaHDT)
}

source("R/cCFR_calculation.R")
source("R/plotting_functions.R")

ChinaData <- subset(allTogetherInternational, country == "China")
JapanData <- subset(allTogetherInternational, country == "Japan")
KoreaData <- subset(allTogetherInternational, country == "South Korea")
IranData  <- subset(allTogetherInternational, country == "Iran")
ItalyData <- subset(allTogetherInternational, country == "Italy")


master_plot(allTogetherInferred, 1, 48, "topleft", hospitalisation_to_death)
master_plot(JapanData, 1, 1, "top", hospitalisation_to_death)
master_plot(KoreaData, 1, 1, "topright", hospitalisation_to_death)
master_plot(IranData, 1, 1, "right", hospitalisation_to_death)
master_plot(ItalyData, 1, 1, "topright", hospitalisation_to_death)


master_plot(allTogetherInferred, 1, 48, "topleft", hospitalisation_to_death_truncated)
master_plot(ChinaData, 15, 16, "bottomright", hospitalisation_to_death_truncated)
master_plot(JapanData, 1, 24, "top", hospitalisation_to_death_truncated)
master_plot(KoreaData, 1, 17, "topright", hospitalisation_to_death_truncated)
master_plot(cruise_ship_by_confmation, 1, 16, "topright", hospitalisation_to_death_truncated)

plotChina


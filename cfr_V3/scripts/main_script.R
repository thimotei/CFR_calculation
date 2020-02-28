library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)

# read in data

scaled_reporting <- 1.0

source("R/read_dataV2.R")

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

cCFRTimeSeriesReal <- output_cfr_timeseries(allTogetherReal, onset_to_death)
cCFRTimeSeriesReal <- cbind(date=dateRangeRealData, cCFRTimeSeriesReal)
cCFRTimeSeriesRealConstrained <- subset(cCFRTimeSeriesReal, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)
cfrPointEstimateReal <- output_estimates(allTogetherReal, onset_to_death)

cCFRTimeSeriesInferred <- output_cfr_timeseries(allTogetherInferred, onset_to_death)
cCFRTimeSeriesInferred <- cbind(date=dateRangeIncidence, cCFRTimeSeriesInferred)
cCFRTimeSeriesInferredConstrained <- subset(cCFRTimeSeriesInferred, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)
cCFRPointEstimateInferred <- output_estimates(allTogetherInferred, onset_to_death)


nCFRRealCIs <- calculate_CIs_nCFR(cCFRTimeSeriesRealConstrained)
nCFRInferredCIs <- calculate_CIs_nCFR(cCFRTimeSeriesInferredConstrained)

cCFRRealCIs <- calculate_CIs_cCFR(cCFRTimeSeriesRealConstrained)
cCFRInferredCIs <- calculate_CIs_cCFR(cCFRTimeSeriesInferredConstrained)

# international cases without Diamond princess cruise ship included

cCFRTimeSeriesInternational <- output_cfr_timeseries(internationalDataRaw, confirmation_to_death)
cCFRTimeSeriesInternational <- cbind(date=internationalDataRaw$date, cCFRTimeSeriesInternational)
cCFRTimeSeriesInternationalConstrained <- subset(cCFRTimeSeriesInternational, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)

nCFRInternationalCIs <- calculate_CIs_nCFR(cCFRTimeSeriesInternationalConstrained)
cCFRInternationalCIs <- calculate_CIs_cCFR(cCFRTimeSeriesInternationalConstrained)

# international cases with Diamond princess cruise ship included

# combining international data and cruise ship data

relevantInternationalSubset <- subset(internationalDataRaw[c(1,6,7)], 
                                      date >= head(cruiseShipDataRaw$date, n = 1) & 
                                      date <= tail(cruiseShipDataRaw$date, n = 1))

combinedCases <- relevantInternationalSubset$new_cases + cruiseShipDataRaw$new_cases
combinedDeaths <- relevantInternationalSubset$new_deaths + cruiseShipDataRaw$new_deaths

internationalCruiseData <- data.frame(date = relevantInternationalSubset$date,
                           new_cases = combinedCases,
                           new_deaths = combinedDeaths)



cCFRTimeSeriesInternationalCruise <- output_cfr_timeseries(internationalCruiseData, confirmation_to_death)
cCFRTimeSeriesInternationalCruise <- cbind(date=internationalCruiseData$date, cCFRTimeSeriesInternationalCruise)
cCFRTimeSeriesInternationalCruiseConstrained <- subset(cCFRTimeSeriesInternationalCruise,
                                                       known_outcomes > deaths & deaths > 0 & known_outcomes > 0)
#cfrPointEstimateInternational <- output_estimates(cCFRTimeSeriesInternationalConstrained)

nCFRInternationalCruiseCIs <- calculate_CIs_nCFR(cCFRTimeSeriesInternationalCruiseConstrained)
cCFRInternationalCruiseCIs <- calculate_CIs_cCFR(cCFRTimeSeriesInternationalCruiseConstrained)


# cruise ship cases only

cCFRTimeSeriesCruise <- output_cfr_timeseries(cruiseShipDataRaw, confirmation_to_death)
cCFRTimeSeriesCruise <- cbind(date=cruiseShipDataRaw$date, cCFRTimeSeriesCruise)
cCFRTimeSeriesCruiseConstrained <- subset(cCFRTimeSeriesCruise, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)
#cfrPointEstimateInternational <- output_estimates(cCFRTimeSeriesInternationalConstrained)

nCFRInternationalJustCruiseCIs <- calculate_CIs_nCFR(cCFRTimeSeriesCruiseConstrained)
cCFRInternationalustCruiseCIs <- calculate_CIs_cCFR(cCFRTimeSeriesCruiseConstrained)


# cruise ship cases only

cCFRTimeSeriesCruise <- output_cfr_timeseries(cruiseShipDataRaw, confirmation_to_death)
cCFRTimeSeriesCruise <- cbind(date=cruiseShipDataRaw$date, cCFRTimeSeriesCruise)
cCFRTimeSeriesCruiseConstrained <- subset(cCFRTimeSeriesCruise, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)
#cfrPointEstimateInternational <- output_estimates(cCFRTimeSeriesInternationalConstrained)

nCFRInternationalJustCruiseCIs <- calculate_CIs_nCFR(cCFRTimeSeriesCruiseConstrained)
cCFRInternationalustCruiseCIs <- calculate_CIs_cCFR(cCFRTimeSeriesCruiseConstrained)

# running Christian's data and distribution as a check

christiansData <- read.csv("data/ncov_cases_20200217.csv")

names(christiansData)[names(christiansData) == "cases"] <- "new_cases"
names(christiansData)[names(christiansData) == "deaths"] <- "new_deaths"

cCFRTimeSeriesChristian <- output_cfr_timeseries(christiansData, linton_dist)
cCFRTimeSeriesChristian <- cbind(date=christiansData$date, cCFRTimeSeriesChristian)
cCFRTimeSeriesChristianConstrained <- subset(cCFRTimeSeriesChristian, known_outcomes > deaths & deaths > 0 & known_outcomes > 0)

nCFRChristianCIs <- calculate_CIs_nCFR(cCFRTimeSeriesChristianConstrained)
cCFRChristianCIs <- calculate_CIs_cCFR(cCFRTimeSeriesChristianConstrained)

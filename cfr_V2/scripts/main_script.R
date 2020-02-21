library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)

# read in data

## Why is this 1 and not tested. In general global enviornment variables are very bad practice.
scaled_reporting <- 1.0

source("R/read_data.R")

######### parameterising the delay distributions (taken from papers) ############

linton <- read.csv("data/linton_supp_tableS1_S2_8Feb2020.csv")
linton <- dmy(linton$Death) - dmy(linton$Onset)
linton <- as.numeric(na.omit(linton))
fit_linton <- fitdist(linton, "gamma")

imperial <- read.csv("data/hubei_early_deaths_2020_07_02.csv")
imperial <- dmy(imperial$date_death) - dmy(imperial$date_onset)
imperial <- as.numeric(na.omit(imperial))
fit_imperial <- fitdist(imperial, "gamma")



shapeLinton = 5.099735
rateLinton = 0.3386777

linton_dist <- function(x)
{
  dgamma(x, shapeLinton, rateLinton)
}


meanG <- 13.8 # subtract delay from onset-to-confirmation
scaleG <- 1

onset_to_death <- function(x)
{
  dgamma(x, meanG/scaleG, scale = scaleG)
}


shapeCD = 1.7205
scaleCD = 9.9827

confirmation_to_death <- function(x)
{
  dweibull(x, shapeCD, scaleCD)
}


########## computing the corrected CFR trajectories and confidence intervals

source("R/cCFR_calculation.R")

cCFRTimeSeriesReal <- output_cfr_timeseries(allTogetherReal, onset_to_death)
cCFRTimeSeriesReal <- cbind(date=dateRangeRealData, cCFRTimeSeriesReal)
cCFRTimeSeriesRealConstrained <- subset(cCFRTimeSeriesReal, 
                                        known_outcomes > deaths & 
                                          deaths > 0 &
                                          known_outcomes > 0)
cfrPointEstimateReal <- output_estimates(allTogetherReal, onset_to_death)

cCFRTimeSeriesInferred <- output_cfr_timeseries(allTogetherInferred, onset_to_death)
cCFRTimeSeriesInferred <- cbind(date=dateRangeIncidence, cCFRTimeSeriesInferred)
cCFRTimeSeriesInferredConstrained <- subset(cCFRTimeSeriesInferred, 
                                            known_outcomes > deaths & 
                                              deaths > 0 & 
                                              known_outcomes > 0)
cCFRPointEstimateInferred <- output_estimates(allTogetherInferred, onset_to_death)


nCFRRealCIs <- calculate_CIs_nCFR(cCFRTimeSeriesRealConstrained)
nCFRInferredCIs <- calculate_CIs_nCFR(cCFRTimeSeriesInferredConstrained)

cCFRRealCIs <- calculate_CIs_cCFR(cCFRTimeSeriesRealConstrained)
cCFRInferredCIs <- calculate_CIs_cCFR(cCFRTimeSeriesInferredConstrained)

# international cases without Diamond princess cruise ship included

cCFRTimeSeriesInternational <- output_cfr_timeseries(internationalDataRaw, 
                                                     confirmation_to_death)
cCFRTimeSeriesInternational <- cbind(date=internationalDataRaw$date, 
                                     cCFRTimeSeriesInternational)
cCFRTimeSeriesInternationalConstrained <- subset(cCFRTimeSeriesInternational, 
                                                 known_outcomes > deaths & 
                                                   deaths > 0 &
                                                   known_outcomes > 0)

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



cCFRTimeSeriesInternationalCruise <- output_cfr_timeseries(internationalCruiseData, 
                                                           confirmation_to_death)
cCFRTimeSeriesInternationalCruise <- cbind(date = internationalCruiseData$date,
                                           cCFRTimeSeriesInternationalCruise)
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

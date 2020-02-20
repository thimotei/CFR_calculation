caseDataRaw <- readRDS("~/Documents/lshtm/nCoV2019/case_data/hubei_confirmed.rds")
deathDataRaw <- readRDS("~/Documents/lshtm/nCoV2019/case_data/all_death_prf.rds")
load("~/Documents/lshtm/github repos/2020-ncov/stoch_model/outputs/bootstrap_fit_1.RData")
internationalDataRaw <-  data.frame(read.csv("~/Documents/lshtm/CFR/data/international_cases_deaths.csv"))
cruiseShipDataRaw <-  data.frame(read.csv("~/Documents/lshtm/CFR/data/cruise_ship_diamond_princess_by_confirmation.csv"))


# changing dates from factors to dates
internationalDataRaw$date <- as.Date(internationalDataRaw$date)
cruiseShipDataRaw$date <- as.Date(cruiseShipDataRaw$date)

#earlyOutbreak <- data.frame(read.csv("~/Documents/lshtm/CFR/data/early_outbreak.csv"), row.names = NULL)

inferredDataIncidence <- read.csv("data/case_model.csv")
#inferredDataIncidence <- data.frame(date = inferredDataCumulative$date[2:nrow(inferredDataCumulative)],
#                                    new_cases = diff(inferredDataCumulative$cases))

realCaseDataCumulative <- data.frame(date = caseDataRaw$date[caseDataRaw$CNTY_CODE==420100], 
                                     cases = caseDataRaw$total_case[caseDataRaw$CNTY_CODE==420100])
realDeathDataCumulative <- data.frame(date = deathDataRaw$date[deathDataRaw$CNTY_CODE==420100],
                                      deaths = deathDataRaw$fatality[deathDataRaw$CNTY_CODE==420100])

# fixing missing data for deaths

firstDateInferred <- head(inferredDataIncidence$date, n=1)
lastDateInferred <- tail(inferredDataIncidence$date, n=1)
firstDateDeath <- head(realDeathDataCumulative$date, n=1)

dateRangeUnknownDeaths <- seq(as.Date(firstDateInferred), as.Date(firstDateDeath)-1,1)
dateRangeRealData <- seq(as.Date("2020-01-15"),as.Date("2020-02-11"),1)

missingDeathData <- data.frame(date = dateRangeUnknownDeaths,
                               deaths = rep(0,length(dateRangeUnknownDeaths)))

                               
realDeathDataCumulative <- rbind(missingDeathData,realDeathDataCumulative)

realDeathDataCumulative$deaths[51:54] <- 1
realDeathDataCumulative$deaths[56:57] <- 2


# fixing missing data for cases

# calculating real incidence of cases and deaths
extra_entry <- data.frame(date = as.Date("2020-01-20"), cases = 240)
# add missing row in case data
realCaseDataCumulative <- rbind(realCaseDataCumulative, extra_entry)
# reordering the time series by date
realCaseDataCumulative <- realCaseDataCumulative[order(as.Date(realCaseDataCumulative$date, format="%d/%m/%Y")),]

# now adding NAs for the entire time period before 15-01-2020 where we are unsure of data

firstDateCases <- head(realCaseDataCumulative$date, n=1)
dateRangeUnknownCases <- seq(as.Date(firstDateInferred), as.Date(firstDateCases) - 1,1)

missingCaseData <- data.frame(date = dateRangeUnknownCases,
                               cases = rep(NA, length(dateRangeUnknownCases)))

realCaseDataCumulative <- rbind(missingCaseData,realCaseDataCumulative)
realCaseDataCumulative <- realCaseDataCumulative[2:83,]

dateRangeIncidence <- seq(as.Date(firstDateInferred) + 1 , as.Date(lastDateInferred),1)
dateRange <- seq(as.Date(firstDateInferred), as.Date(lastDateInferred),1)

# at this point, the three time series are realCaseDataCumulative, realDeathDataCumulative and inferredCaseDataCumulative

# we now combine these into two data frames. One with real cases and real deaths and another with inferred cases and real deaths

allTogetherReal <- data.frame(date = dateRangeIncidence,
                              cases = realCaseDataCumulative$cases[2:82],
                              deaths = realDeathDataCumulative$deaths[2:82],
                              new_cases = diff(realCaseDataCumulative$cases),
                              new_deaths = diff(realDeathDataCumulative$deaths))


allTogetherInferred <- data.frame(date = dateRangeIncidence,
                                  cases = cumsum(inferredDataIncidence$cases)[2:82],
                                  deaths = realDeathDataCumulative$deaths[2:82],
                                  new_cases = inferredDataIncidence$cases[2:82],
                                  new_deaths = diff(realDeathDataCumulative$deaths))

rangeRealCases <- range(allTogetherReal$new_cases[is.na(allTogetherReal$new_cases)==FALSE])


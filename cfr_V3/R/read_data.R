#importing data

wuhanInferredData <- read.csv("data/wuhan_data/inferred_data.csv")
cruise_ship_by_confmation <- read.csv("data/cruise_ship_data/cruise_ship_diamond_princess_by_confirmation.csv")
international_data <- read.csv("data/international_data/bno_scraped_data.csv")
international_data_no_country <- read.csv("data/international_data/bno_scraped_international.csv")
deathDataRaw <- readRDS("data/wuhan_data/all_death_prf.rds")

# importing the age-stratified scale factors
cruise_ship_SFs <- read.csv("data/cruise_ship_data/age_scale_factors.csv")
wuhan_SFs <- read.csv("data/wuhan_data/age_scale_factors.csv")

cruise_ship_by_confmation$date <- as.Date(cruise_ship_by_confmation$date)

# cleaning up the full international data, so its in the correct format
international_data$date <- as.Date(format(as.POSIXct(international_data$date,format="%m/%d/%y %H:%M"), "%Y-%m-%d"))
international_data <- international_data[order(as.Date(international_data$date)),]
countries <- droplevels(unique(international_data$country))
colnames(international_data)[which(names(international_data) == "new_case")] <- "new_cases"
colnames(international_data)[which(names(international_data) == "new_death")] <- "new_deaths"

international_data_no_country$date <- as.Date(format(as.POSIXct(international_data_no_country$date,format="%m/%d/%y %H:%M"), "%Y-%m-%d"))
international_data_no_country <- international_data_no_country[order(as.Date(international_data_no_country$date)),]
colnames(international_data_no_country)[which(names(international_data_no_country) == "new_case")] <- "new_cases"
colnames(international_data_no_country)[which(names(international_data_no_country) == "new_death")] <- "new_deaths"

new_cases_daily <- aggregate(new_cases~date+country, data=international_data, sum, na.rm=TRUE)
new_deaths_daily <- aggregate(new_deaths~date+country, data=international_data, sum, na.rm=TRUE)
new_cases_daily <- new_cases_daily[!new_cases_daily$country=="",]
new_deaths_daily <- new_deaths_daily[!new_deaths_daily$country=="",]

new_cases_daily  <- pad(new_cases_daily[order(as.Date(new_cases_daily$date, format="%Y-%m-%d")),])
new_deaths_daily <- pad(new_deaths_daily[order(as.Date(new_deaths_daily$date, format="%Y-%m-%d")),])

allTogetherInternational <- merge(new_cases_daily, new_deaths_daily)

# cleaning up the simplified international data, so its in the correct format
international_data$date <- as.Date(format(as.POSIXct(international_data$date,format="%m/%d/%y %H:%M"), "%Y-%m-%d"))
international_data <- international_data[order(as.Date(international_data$date)),]
colnames(international_data)[which(names(international_data) == "new_case")] <- "new_cases"
colnames(international_data)[which(names(international_data) == "new_death")] <- "new_deaths"


# age distribution of cruise ship individuals 
cruise_ship_ages <- c(16,23,347, 428,334,398,923,1015,216,11)


# cleaning up Wuhan data, combining the inferred case data with the real death data

dateRangeInferred <- seq(min(as.Date(wuhanInferredData$date)), max(as.Date(wuhanInferredData$date)), 1)
realDataSameDateRange <- subset(allTogetherInternational, country =="China" &
                                min(dateRangeInferred) <= date & date <= max(dateRangeInferred))

# fixing missing data for deaths


wuhanDeathData <- data.frame(date = subset(deathDataRaw, CNTY_CODE == 420100)$date, 
                             deaths = subset(deathDataRaw, CNTY_CODE == 420100)$fatality)

firstDateInferred <- head(wuhanInferredData$date, n=1)
lastDateInferred <- tail(wuhanInferredData$date, n=1)
firstDateDeath <- head(wuhanDeathData$date, n=1)

dateRangeUnknownDeaths <- seq(as.Date(firstDateInferred), as.Date(firstDateDeath)-1,1)
dateRangeRealData <- seq(as.Date("2020-01-15"),as.Date("2020-02-11"),1)

missingDeathData <- data.frame(date = dateRangeUnknownDeaths,
                               deaths = rep(0,length(dateRangeUnknownDeaths)))


wuhanDeathData <- rbind(missingDeathData,wuhanDeathData)

wuhanDeathData$deaths[51:54] <- 1
wuhanDeathData$deaths[56:57] <- 2

wuhanDeathDataIncidence <- diff(wuhanDeathData$deaths)
dateRangeIncidence <- wuhanInferredData$date[2:length(wuhanInferredData$date)]


allTogetherInferred <- data.frame(date  = dateRangeIncidence,
                             new_cases  = wuhanInferredData$cases[2:length(wuhanInferredData$cases)],
                             new_deaths = wuhanDeathDataIncidence)

allTogetherInferred$date <- as.Date(allTogetherInferred$date)

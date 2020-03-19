library(tidyverse)
library(dplyr)
library(padr)

deathDates <- c(as.Date("2020-02-05"), as.Date("2020-02-06"))

johnsData <- read_csv("data/johns_data.csv")
johnsData$date_onset <- as.Date(johnsData$date_onset,format = "%d/%m/%Y")

drop.cols = c("X4", "X5", "X6")

firstDateKnown <- min(subset(johnsData, is.na(date_onset)==FALSE)$date_onset)
lastDateKnown <- max(subset(johnsData, is.na(date_onset)==FALSE)$date_onset) %>% max
dateRangeKnown <- seq(firstDateKnown, lastDateKnown, 1)

dataUnknownDates <- johnsData %>% subset(., is.na(date_onset) == TRUE)
dataKnownDates <- johnsData %>% subset(., is.na(date_onset) == FALSE)
dateRangeKnown <- seq(firstDateKnown, lastDateKnown, by="day")
sampledDates <- sample(dateRangeKnown, dataUnknownDates$date_onset %>% length)
dataUnknownDates$date_onset <- sampledDates
dataPopulatedDates <- full_join(dataUnknownDates, dataKnownDates)

dateRangeNew <- seq(lastDateKnown + 1 , as.Date("2020-03-06", format = "%Y-%m-%d"), by = "day")

johnsDataComplete <- arrange(dataPopulatedDates, date_onset)
newCasesDF <- johnsDataComplete %>% group_by(date_onset) %>% summarise(new_cases = n()) %>% pad
newCasesDF$new_cases[is.na(newCasesDF$new_cases) == TRUE] <- 0
newCasesDFTmp <- as_tibble(data.frame(date = newCasesDF$date_onset, new_cases = newCasesDF$new_cases))


temp <- as_tibble(data.frame(date = dateRangeNew, new_cases = 0, new_deaths = 0))
temp$new_deaths[temp$date == "2020-03-05"] <- 1
temp$new_deaths[temp$date == "2020-03-06"] <- 1

temp$new_cases[temp$date == "2020-03-05"] <- 10
temp$new_cases[temp$date == "2020-03-06"] <- 20

tmp <- full_join(newCasesDFTmp,temp)
tmp$new_deaths[is.na(tmp$new_deaths == TRUE)] <- 0

allTogether <- tmp


cleanData <- function(data_in_messy, omitNAs)
{
  if(omitNAs == TRUE)
  {
    data_in_no_NAs <- data_in_messy[!is.na(data_in_messy$date_onset),] 
    dateRange <- seq(min(as.Date(data_in_messy$date_onset)))
  }
  
}







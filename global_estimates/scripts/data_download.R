suppressPackageStartupMessages({
  require(httr)
  require(readr)
  require(dplyr)
  require(tidyr)
  require(lubridate)
  require(padr)
})

.args <- if (interactive()) c(
  "somedata.csv"
) else commandArgs(trailingOnly = TRUE)

rawthing <- GET(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  authenticate(":", ":", type="ntlm")
)$content

# TODO: not so fast...need to filter as well
allDat <- read_csv(rawthing)

# filter, etc
# munging data into correct format and selecting countries with greater than 10 deaths
allTogetherClean <- allDat %>% 
  arrange(countriesAndTerritories, dateRep) %>% 
  mutate(dateRep = dmy(dateRep))%>% 
  rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories, country_code = countryterritoryCode) %>%
  select(date, country, country_code, new_cases, new_deaths) %>%
  filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  group_by(country) %>%
  pad() %>%
  mutate(new_cases = replace_na(new_cases, 0),
         new_deaths = replace_na(new_deaths, 0)) %>%
  group_by(country) %>%
  mutate(cum_deaths = sum(new_deaths)) %>%
  filter(cum_deaths > 0) %>%
  select(-cum_deaths) %>%
  mutate(new_cases = case_when(new_cases < 0 ~ 0,
                               new_cases >= 0 ~ new_cases),
         new_deaths = case_when(new_deaths < 0 ~ 0,
                                new_deaths >= 0 ~ new_deaths)
         )

# Plot rough reporting over time -----------------------------------------
plot_country_names <- allTogetherClean %>% 
  mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  filter(death_cum_sum >= 10) %>%
  mutate(max_deaths = max(death_cum_sum)) %>% 
  group_by(country) %>%
  summarise(max_deaths = first(max_deaths),
     observations = n()) %>%
  filter(observations >= 10) %>%
  arrange(-max_deaths) %>% 
  pull(country) %>%
  unique()

write_csv(allTogetherClean %>% filter(country %in% plot_country_names), path = tail(.args,1))

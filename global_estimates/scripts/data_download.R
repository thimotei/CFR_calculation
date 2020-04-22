suppressPackageStartupMessages({
  require(httr)
})

.args <- if (interactive()) c(
  "somedata.csv"
) else commandArgs(trailingOnly = TRUE)

rawthing <- GET(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  authenticate(":", ":", type="ntlm")
)$content

# TODO: not so fast...need to filter as well
allDat <- readr::read_csv(rawthing)

# filter, etc
# munging data into correct format and selecting countries with greater than 10 deaths
allTogetherClean <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)


# Plot rough reporting over time -----------------------------------------
plot_country_names <- allTogetherClean %>% 
  dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  dplyr::filter(death_cum_sum >= 10) %>%
  dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise(max_deaths = dplyr::first(max_deaths),
            observations = dplyr::n()) %>%
  dplyr::filter(observations >= 10) %>%
  dplyr::arrange(-max_deaths) %>% 
  dplyr::pull(country) %>%
  unique()

write_csv(allTogetherClean %>% filter(country %in% plot_country_names), file = tail(.args,1))

# Code to fit GAMs to time-series of under-reporting estimates

# Set paths
here::here() %>% setwd()

#source data processing and plotting scripts
source("CFR_calculation/global_estimates/temporal/R/cfr_plot_theme.R")
source("CFR_calculation/global_estimates/temporal/R/get_one_timeseries.R")
source("CFR_calculation/global_estimates/temporal/R/get_plot_data.R")
source("CFR_calculation/global_estimates/temporal/R/plot_country.R")
source("CFR_calculation/global_estimates/temporal/R/scale_CFR_temporal.R")


# Set parameters
zMean <- 13
zSD <- 12.7
zMedian <- 9.1
mu <- log(zMedian)
sigma <- sqrt(2*(log(zMean) - mu))


# set baseline level CFR
cCFRBaseline <- 1.4
cCFREstimateRange <- c(1.2, 1.7)

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}


# Load data -----------------------------------------------------
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- readr::read_csv(tf)


allTogether <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan")) %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  #What is this doing?
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths)


# Plot rough reporting over time -----------------------------------------
plot_country_names <- allTogether %>% 
  dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
  dplyr::filter(death_cum_sum >= 10) %>% 
  dplyr::mutate(max_deaths = max(death_cum_sum)) %>% 
  dplyr::arrange(-max_deaths) %>% 
  dplyr::group_by(country) %>% 
  dplyr::filter(dplyr::n() >= 8) %>%
  dplyr::pull(country) %>% 
  unique()

plot_data <- get_plot_data(country_name = "Spain")

cfr_plots <- list()
for (country_name in plot_country_names){
  plot_data <- get_plot_data(country_name = country_name)
  
  p <- try(plot_country(plot_data = plot_data))
  
  if ('try-error' %in% class(p)){next}
  
  cfr_plots[[country_name]] = p
  
}

cfr_plot_grid = gridExtra::arrangeGrob(grobs = cfr_plots,
                            ncol = 4,
                            left = "Percentage of symptomatic cases reported", 
                            rot = 90)


ggplot2::ggsave('CFR_calculation/global_estimates/outputs/figure_1.png',
       cfr_plot_grid,
       width =11, 
       height = 25, 
       units = 'in', 
       dpi = 450)


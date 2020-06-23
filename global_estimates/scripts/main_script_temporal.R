# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

# Set paths
setwd(here::here())

#source data processing and plotting scripts
source('CFR_calculation/global_estimates/temporal/R/get_plot_data.R')
source('CFR_calculation/global_estimates/temporal/R/plot_country.R')
source('CFR_calculation/global_estimates/temporal/R/run_bayesian_model.R')
source('CFR_calculation/global_estimates/temporal/R/cfr_plot_theme.R')
source('CFR_calculation/global_estimates/temporal/R/scale_cfr_temporal.R')

# setting baseline level CFR
CFRBaseline <- 1.4
CFREstimateRange <- c(1.2, 1.7)

# Set parameters
mean <- 13
median <- 9.1

mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}

# Load data -----------------------------------------------------
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- readr::read_csv(tf)

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


# running loop over all countries, fitting the model, saving the fit data and making each plot
cfr_plots <- list()
for (country_name in plot_country_names){
  tryCatch({ 
    
    plot_data <- get_plot_data(country_name = plot_country_names, CFRBaseline = CFRBaseline)
    prediction <- run_bayesian_model(plot_data)
    
    saveRDS(prediction, paste0("outputs/fit_data/",country_name, "_fit" ,'.rds'))
    
    ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                              y = c(prediction$upper, rev(prediction$lower)))
    
    p <- try(plot_country(plot_data, prediction$estimate, ci_poly))
    
    if ('try-error' %in% class(p)){next}
    
    ggplot2::ggsave(paste0("outputs/cfr_plots/", country_name, "_plot.pdf"),
           p,
           width = 8, 
           height = 10, 
           units = 'in', 
           useDingbats = FALSE,
           dpi = 400)
    
    cfr_plots[[country_name]] = p 
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

plot_data <- get_plot_data(country_name = "Spain", CFRBaseline = CFRBaseline)

# arranging all of the plots in cfr_plots
cfr_plot_grid = gridExtra::arrangeGrob(grobs = cfr_plots, ncol = 1)

# saving all of the plots as a .png, to make loading time on the .html not too long
ggplot2::ggsave('./outputs/cfr_plots/cfr_plot_grid.png',
       cfr_plot_grid,
       width = 8, 
       height = 10, 
       units = 'in', 
       dpi = 400)



  

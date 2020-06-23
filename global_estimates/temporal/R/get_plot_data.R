#get time varying cfr data for a country
get_plot_data <- function(country_name, data = allTogetherClean, CFRBaseline){

#filter country data and adjust date
  country_data <- data %>% 
    dplyr::filter(country == country_name) %>% 
    dplyr::mutate(date = date - mean)
  
  #date where cumulative deaths passed 10
  death_threshold_date <- country_data %>% 
    dplyr::mutate(death_cum_sum = cumsum(new_deaths)) %>% 
    dplyr::filter(death_cum_sum >= 10) %>% 
    dplyr::pull(date) %>% 
    min()

  # date where negative death reporting spikes occur
  reporting_spike <- country_data %>%
    dplyr::filter(new_deaths < 0) %>%
    dplyr::pull(date) %>%
    min()
  
  #return adjusted date and reporting_estimate
  cfr <- scale_cfr_temporal(country_data) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(reporting_estimate = CFRBaseline/cCFR) %>% 
    dplyr::mutate(reporting_estimate = pmin(reporting_estimate, 1),
           country = country_data$country,
           date = country_data$date,
           date_num = as.numeric(country_data$date),
           deaths = country_data$new_deaths,
           cases_known = cum_known_t) %>% 
    dplyr::filter(date >= death_threshold_date) %>% 
    dplyr::filter(date < reporting_spike) %>% 
    dplyr::select(country, date, date_num, reporting_estimate, deaths, cases_known)
  
  return(cfr)
  
}


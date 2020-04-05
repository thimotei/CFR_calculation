#get time varying cfr data for a country
get_plot_data <- function(country_name, data = allTogetherCleanA){
  
  true_cfr <- 1.4/100
  
  #filter country data and adjust date
  country_data <- data %>% 
    filter(country == country_name) %>% 
    mutate(date = date - zmeanHDT)
  
  #date where cumulative deaths passed 10
  death_threshold_date <- country_data %>% 
    mutate(death_cum_sum = cumsum(new_deaths)) %>% 
    filter(death_cum_sum >= 10) %>% 
    pull(date) %>% 
    min()

  #return adjusted date and reporting_estimate
  cfr <- scale_cfr_temporal(country_data) %>% 
    as_tibble() %>% 
    mutate(reporting_estimate = true_cfr/cCFR) %>% 
    mutate(reporting_estimate = pmin(reporting_estimate, 1),
           country = country_data$country,
           date = country_data$date,
           date_num = as.numeric(country_data$date),
           deaths = country_data$new_deaths,
           cases_known_adj = cum_known_t * true_cfr) %>% 
    filter(date >= death_threshold_date) %>% 
    select(country, date, date_num, reporting_estimate, deaths, cases_known_adj)
  
  return(cfr)
  
}
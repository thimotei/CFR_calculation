jhu_data_import <- function() 
{
  #--- setting the urls for the raw data. cases and deaths are separate in the 
  #--- raw data
  jhu_cases_path <- "https://raw.githubusercontent.com/cssegisanddata/covid-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  jhu_deaths_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  
  #--- reading in, melting and cleaning the case data
  jhu_cases_dt <- data.table::fread(jhu_cases_path) %>% 
    .[, c("Province/State", "Lat", "Long") := NULL] %>% 
    melt(., 
         id.vars = "Country/Region",
         variable.name = "date", 
         value.name = "new_cases") %>% 
    setnames(., 
             old = "Country/Region",
             new = "country") %>% 
    .[, date := mdy(date)] %>% 
    .[, new_cases := sum(as.numeric(new_cases)), by = c("date", "country")] %>%
    unique() %>% 
    .[, new_cases := new_cases - shift(new_cases), by = "country"] %>% 
    .[new_cases < 0, new_cases := 0] %>% 
    .[is.na(new_cases) == FALSE] %>% 
    .[, iso3c := countrycode(country,
                             'country.name',
                             'iso3c',
                             custom_match = c("Micronesia" = "FSM",
                                              "Kosovo" = "XXK"))] %>% 
    .[is.na(iso3c) == FALSE] %>% 
    setcolorder("iso3c") %>% 
    .[order(country, date)] %>% 
    unique()
  
  #--- reading in, melting and cleaning the death data
  jhu_deaths_dt <- data.table::fread(jhu_deaths_path) %>% 
    .[, c("Province/State", "Lat", "Long"):=NULL] %>% 
    melt(., 
         id.vars = "Country/Region",
         variable.name = "date", 
         value.name = "new_deaths") %>% 
    setnames(., 
             old = "Country/Region",
             new = "country") %>% 
    .[, date := mdy(date)] %>% 
    .[, new_deaths := sum(as.numeric(new_deaths)), by = c("date", "country")] %>% 
    unique() %>% 
    .[, new_deaths := new_deaths - shift(new_deaths), by = "country"] %>% 
    .[new_deaths < 0, new_deaths := 0] %>% 
    .[is.na(new_deaths) == FALSE] %>% 
    .[, iso3c := countrycode(country,
                             'country.name',
                             'iso3c',
                             custom_match = c("Micronesia" = "FSM",
                                              "Kosovo" = "XXK"))] %>% 
    .[is.na(iso3c) == FALSE] %>% 
    setcolorder("iso3c") %>% 
    .[order(country, date)] %>% 
    unique()
  
  #--- combining the data.tables into a final big data.table
  jhu_full_dt <- merge.data.table(jhu_cases_dt,
                                  jhu_deaths_dt,
                                  by = c("date", "iso3c", "country"), 
                                  all = FALSE) %>% 
    .[order(country, date)]
  
  return(jhu_full_dt)
}
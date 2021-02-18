case_death_timeseries_function <- function(regional = FALSE)
{
    ### downloading and cleaning covid-19 case time-series data from johns hopkins
    ### university database

    jhu_cases_path <- "https://raw.githubusercontent.com/cssegisanddata/covid-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

    jhu_cases_raw <- data.table::fread(jhu_cases_path)

    jhu_cases_df  <- jhu_cases_raw %>%
        dplyr::select(-Lat, -Long) %>%
        tidyr::pivot_longer(!c("Province/State", "Country/Region"), 
                            names_to = "date", 
                            values_to = "cases") %>%
        dplyr::na_if("") %>%
        dplyr::rename(province = "Province/State",
                      country = "Country/Region") %>%
        dplyr::mutate(province = dplyr::case_when(is.na(province) == TRUE ~
                                                          "National",
                                                      is.na(province) == FALSE ~ province)) %>%
        dplyr::mutate(iso3c = countrycode::countrycode(country,
                                                       'country.name',
                                                       'iso3c',
                      custom_match = c("Micronesia" = "FSM", "Kosovo" = "XXK"))) %>%
        #dplyr::mutate(iso3c = countrycode::countrycode(country, 'country.name', 'iso3c')) %>%
        dplyr::select(date, country, iso3c, province, cases)


    ### downloading and cleaning covid-19 death time-series data from johns hopkins
    ### university database

    jhu_deaths_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

    jhu_deaths_raw <- data.table::fread(jhu_deaths_path)

    jhu_deaths_df  <- jhu_deaths_raw %>%
        dplyr::select(-Lat, -Long) %>%
        tidyr::pivot_longer(!c("Province/State", "Country/Region"), 
                            names_to = "date", 
                            values_to = "deaths") %>%
        dplyr::na_if("") %>%
        dplyr::rename(province = "Province/State",
                      country = "Country/Region") %>%
        dplyr::mutate(province = dplyr::case_when(is.na(province) == TRUE ~
                                                          "National",
                                                      is.na(province) == FALSE ~ province)) %>%
        dplyr::mutate(iso3c = countrycode::countrycode(country,
                                                       'country.name',
                                                       'iso3c',
                      custom_match = c('Micronesia' = 'FSM', "Kosovo" = "XXK"))) %>%
        dplyr::select(date, country, iso3c, province, deaths)

        jhu_data_conditional <- jhu_cases_df %>%
            dplyr::left_join(jhu_deaths_df) %>%
            dplyr::mutate(date = lubridate::mdy(date)) %>%
            padr::pad() %>%
            dplyr::rename(new_cases = cases, new_deaths = deaths) %>%
            dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                   new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
            dplyr::group_by(country) %>%
            dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
            dplyr::filter(cum_deaths > 0) %>%
            dplyr::select(-cum_deaths) %>%
            dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0,
                                         new_cases >= 0 ~ new_cases),
                   new_deaths = dplyr::case_when(new_deaths < 0 ~ 0,
                                          new_deaths >= 0 ~ new_deaths))

    if(regional == TRUE)
    {
        jhu_data <- jhu_data_conditional %>%
        dplyr::arrange(country, date) 
    }
    else
    {
        jhu_data <- jhu_data_conditional %>%
            dplyr::filter(province == "National") %>%
            dplyr::select(-province) %>%
            dplyr::arrange(country, date)
    }
    return(jhu_data)
}



case_death_timeseries_function <- function()
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
        dplyr::mutate(iso3c = countrycode::countrycode(country, 'country.name', 'iso3c')) %>%
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
        dplyr::mutate(iso3c = countrycode::countrycode(country, 'country.name', 'iso3c')) %>%
        dplyr::select(date, country, iso3c, province, deaths)

    jhu_data <- jhu_cases_df %>%
        dplyr::left_join(jhu_deaths_df)

    return(jhu_data)

}

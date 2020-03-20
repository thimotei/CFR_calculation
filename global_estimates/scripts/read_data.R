allDat <- NCoVUtils::get_ecdc_cases()
allDatDesc <- allDat %>% dplyr::arrange(country, date)
allDatDesc <- allDatDesc %>% dplyr::mutate(date = lubridate::ymd(date)) %>% dplyr::rename(new_cases = cases,
                                                                                          new_deaths = deaths)
                            
allDatDesc <- allDatDesc %>% dplyr::select(date, country, new_cases, new_deaths)

countries <- allDatDesc$country %>% unique


#library(magrittr)
#library(tidyverse)

international_case_counts = function(){
  base_url = 'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-'
  url = paste0(base_url, as.Date(Sys.time()) - 1, '.xls')
  download.file(url, destfile="data/international_case_counts.xls", mode='wb')
}
international_case_counts()
allDat <- readxl::read_xls("data/international_case_counts.xls")

allDat <- allDat %>% dplyr::rename(country = `Countries and territories`)
allDatDesc <- allDat %>% dplyr::arrange(country, DateRep)
allDatDesc <- allDatDesc %>% dplyr::mutate(DateRep = lubridate::ymd(DateRep)) %>% dplyr::rename(date = DateRep,
                                                                                                new_cases  = Cases,
                                                                                                new_deaths = Deaths)
allDatDesc <- allDatDesc %>% dplyr::select(date, country, new_cases, new_deaths)

countries <- allDatDesc$country %>% unique

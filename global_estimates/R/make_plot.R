# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

# Get the data ------------------------------------------------------------

report_data_raw <- readRDS("data/all_together_plot.rds")
total_deaths <- readRDS("data/total_deaths.rds")

# Munge the data ----------------------------------------------------------

report_data <- report_data_raw %>% 
  dplyr::left_join(total_deaths, by = c("country")) %>% 
  dplyr::mutate(
    bottom = quantile2.5,
    lower = quantile25,
    median = quantile50,
    upper = quantile75,
    top  = quantile95.5
  ) %>% 
  dplyr::mutate(
    confidence = dplyr::case_when(total_deaths <= 10 ~ "Countries that have reported fewer than or equal to 10 deaths",
                                  total_deaths < 100 ~ "Countries that have reported fewer than 100 deaths, but more than 10",
                                  total_deaths >= 100 ~ "Countries which have reported 100 or more deaths") %>% 
      factor(levels = c( "Countries which have reported 100 or more deaths", "Countries that have reported fewer than 100 deaths, but more than 10", "Countries that have reported fewer than or equal to 10 deaths"))
  ) %>% 
  dplyr::mutate_if(is.numeric, ~ . / 100) %>% 
  ## Put long country names over two rows
  dplyr::mutate(country = country %>% 
                  stringr::str_replace("United States of America", "United States \n of America") %>% 
                  stringr::str_replace("United Kingdom", "United \n Kingdom"))

 
#report_data <- report_data %>% filter(!stringr::str_detect(country, "CANADA"))
saveRDS(report_data, file = "data/all_together_plot_clean.rds")







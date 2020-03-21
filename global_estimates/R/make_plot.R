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
    confidence = dplyr::case_when(total_deaths < 10 ~ "Fewer than 10 deaths",
                                  total_deaths < 100 ~ "Fewer than 100 deaths",
                                  total_deaths >= 100 ~ "More than (or equal to) 100 deaths") %>% 
      factor(levels = c("More than (or equal to) 100 deaths", "Fewer than 100 deaths", "Fewer than 10 deaths"))
  ) %>% 
  dplyr::mutate_if(is.numeric, ~ . / 100) %>% 
  ## Put long country names over two rows
  dplyr::mutate(country = country %>% 
                  stringr::str_replace("United States of America", "United States \n of America") %>% 
                  stringr::str_replace("United Kingdom", "United \n Kingdom"))

saveRDS(report_data, file = "data/all_together_plot_clean.rds")





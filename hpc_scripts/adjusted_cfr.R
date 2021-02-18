suppressPackageStartupMessages({
  require(readr)
  require(dplyr)
  require(tidyr)
  require(countrycode)
  require(wpp2019)
})

.args <- if (interactive()) c(
  "age_stratified_cfr.csv", "adjusted_cfr.csv"
) else commandArgs(trailingOnly = TRUE)
  
age_stratified_cfr <- read_csv(.args[1])

data(popM)
data(popF)

male_age_strat_age_data <- popM %>% 
  select(country_code, age, "2020") %>%
  mutate(iso3c = countrycode(country_code, "iso3n", destination = 'iso3c')) %>%
  drop_na() %>%
  mutate(age = dplyr::case_when(age == "0-4" ~ "0-9",
                                       age == "5-9" ~ "0-9",
                                       age == "10-14" ~ "10-19",
                                       age == "15-19" ~ "10-19",
                                       age == "20-24" ~ "20-29",
                                       age == "25-29" ~ "20-29",
                                       age == "30-34" ~ "30-39",
                                       age == "35-39" ~ "30-39",
                                       age == "40-44" ~ "40-49",
                                       age == "45-49" ~ "40-49",
                                       age == "50-54" ~ "40-49",
                                       age == "55-59" ~ "50-59",
                                       age == "60-64" ~ "60-69",
                                       age == "65-69" ~ "60-69",
                                       age == "70-74" ~ "70-79",
                                       age == "75-79" ~ "70-79",
                                       age == "80-84" ~ "80+",
                                       age == "85-89" ~ "80+",
                                       age == "90-94" ~ "80+",
                                       age == "95-99" ~ "80+",
                                       age == "100+" ~ "80+")) %>%
  rename(population = "2020") %>%
  group_by(iso3c, age) %>%
  arrange(age) %>%
  summarise(population_male = sum(population))


female_age_strat_age_data <- popF %>% 
  select(country_code, age, "2020") %>%
  mutate(iso3c = countrycode(country_code, "iso3n", destination = 'iso3c')) %>%
  mutate(age = case_when(age == "0-4" ~ "0-9",
                                       age == "5-9" ~ "0-9",
                                       age == "10-14" ~ "10-19",
                                       age == "15-19" ~ "10-19",
                                       age == "20-24" ~ "20-29",
                                       age == "25-29" ~ "20-29",
                                       age == "30-34" ~ "30-39",
                                       age == "35-39" ~ "30-39",
                                       age == "40-44" ~ "40-49",
                                       age == "45-49" ~ "40-49",
                                       age == "50-54" ~ "40-49",
                                       age == "55-59" ~ "50-59",
                                       age == "60-64" ~ "60-69",
                                       age == "65-69" ~ "60-69",
                                       age == "70-74" ~ "70-79",
                                       age == "75-79" ~ "70-79",
                                       age == "80-84" ~ "80+",
                                       age == "85-89" ~ "80+",
                                       age == "90-94" ~ "80+",
                                       age == "95-99" ~ "80+",
                                       age == "100+" ~ "80+")) %>%
  drop_na() %>%
  rename(population = "2020") %>%
  group_by(iso3c, age) %>%
  arrange(age) %>%
  summarise(population_female = sum(population)) 


age_strat_age_data <- male_age_strat_age_data %>%
  left_join(female_age_strat_age_data, by = c("iso3c", "age")) %>% 
  mutate(population_male = population_male*1000,
         population_female = population_female*1000,
         population = population_male + population_female)

age_adjusted_cfr <- age_strat_age_data %>%
  group_by(iso3c) %>%
  summarise(cfr_mid  = weighted.mean(age_stratified_cfr$cfr_mid, population)*100,
            cfr_low  = weighted.mean(age_stratified_cfr$cfr_low, population)*100,
            cfr_high = weighted.mean(age_stratified_cfr$cfr_high, population)*100)

write.csv(age_adjusted_cfr, tail(.args, 1), row.names = FALSE)

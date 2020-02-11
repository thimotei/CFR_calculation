# Load and format data ----------------------------------------------------

# - - -
# Load case & death timeseries
# NOTE: currently stratify by within/outside China

all_dat <- read_csv("~/Documents/lshtm/github repos/CFR_calculation/data/case_death_data_WHO.csv")

# loading inferred infection data from Wuhan outbreak model

start_date <- as.Date("2019-11-22") # first case
end_date <- as.Date("2020-03-01") # period to forecast ahead
t_period <- as.numeric(end_date-start_date)

load("~/Documents/lshtm/github repos/2020-ncov/stoch_model/outputs/bootstrap_fit_1.RData")

incidenceAtT <- diff(C_local_plot)
incidenceAtTDF <- data.frame(incidenceAtT)
incidenceAtTDFNoNA <- incidenceAtTDF %>% select_if(~sum(!is.na(.)) > 0)

# Add extra columns
all_dat <- all_dat %>% mutate(cumulative_cases_china = (cumulative_cases_global - cumulative_cases_outside),
                       cumulative_deaths_global = cumulative_deaths_china + cumulative_deaths_outside,
                       naive_cfr_china = cumulative_deaths_global/cumulative_cases_global
                       )

# - - 
# Add incidence
all_dat <- all_dat %>% mutate(new_cases_china = NA,
                              new_deaths_china = NA,
                              new_sev_cases_china = NA,
                              new_cases_outside = NA,
                              new_deaths_outside = NA
                              )

# Run incidence calculations
all_dat$new_cases_china <- all_dat$cumulative_cases_china - c(0,head(all_dat$cumulative_cases_china,-1))
all_dat$new_deaths_china <- all_dat$cumulative_deaths_china - c(0,head(all_dat$cumulative_deaths_china,-1))
all_dat$new_sev_cases_china <- all_dat$cumulative_sev_cases_china - c(0,head(all_dat$cumulative_sev_cases_china,-1))
all_dat$new_cases_outside <- all_dat$cumulative_cases_outside - c(0,head(all_dat$cumulative_cases_outside,-1))
all_dat$new_deaths_outside <- all_dat$cumulative_deaths_outside - c(0,head(all_dat$cumulative_deaths_outside,-1))



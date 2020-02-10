# Load and format data ----------------------------------------------------

# - - -
# Load case & death timeseries
# NOTE: currently stratify by within/outside China

all_dat <- read_csv("~/repos/CFR_calculation/data/case_death_data_WHO.csv")

# loading inferred infection data from Wuhan outbreak model

start_date <- as.Date("2019-11-22") # first case
end_date <- as.Date("2020-02-") # period to forecast ahead
t_period <- as.numeric(end_date-start_date)+1

inferred_data_wuhan <- load("../stoch_model/outputs/bootstrap_fit_1.RData")
inferred_infection_data = I_plot[,!is.na(I_plot[t_period,])]

# calculating rolling mean of the infection trajectories I(t) from the Wuhan outbreak model

inferred_infection_average <- NULL
for(j in 1:90)
  for(i in 1:100)
  {
    inferred_infection_average[i] <- mean(inferred_infection_data[i,j])
  }


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



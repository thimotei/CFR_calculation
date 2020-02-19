# Load and format data ----------------------------------------------------

# - - -
# Load case & death timeseries
# NOTE: currently stratify by within/outside China

all_dat <- read_csv("data/case_death_data_WHO.csv")

# loading inferred infection data from Wuhan outbreak model

start_date <- as.Date("2019-11-22") # first case
end_date <- as.Date("2020-03-01") # period to forecast ahead
end_date_minus_1 <- as.Date("2020-02-29") # period to forecast ahead
t_period <- as.numeric(end_date-start_date) # for numerical use of the time difference
date_range <- seq(start_date,end_date,1) # creating a vector the time range for plots etc
date_range_minus_1 <- seq(start_date,end_date_minus_1,1) # creating a vector the time range for plots etc

#load("~/repos/2020-ncov/stoch_model/outputs/bootstrap_fit_1.RData")

prevalence <- C_local_plot
incidenceAtT <- diff(C_local_plot)
incidenceAtTDF <- data.frame(incidenceAtT)
incidenceAtTDFMeans <- rowMeans(incidenceAtTDF, na.rm = TRUE)
prevalenceMean <- rowMeans(prevalence, na.rm = TRUE)
cumulativePrevalence <- cumsum(prevalenceMean)

### plotting the data for a sanity check ### 

# plot(x = date_range_minus_1 , y = incidenceAtTDFMeans, xlab = "Date", ylab = "Incidence of Covid-19 in Wuhan")
# plot(x = date_range, y = incidenceCumulative, xlab = "Date", ylab = "Prevalence of Covid-19 in Wuhan")
# plot(x = date_range, y = cumulativePrevalence, xlab = "Date", ylab = "Cumulative cases of Covid-19 in Wuhan")


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



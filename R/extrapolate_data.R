# Load and format data ----------------------------------------------------

# Calculate mean recent growth
extrapolate_data <- function(xx,extrapolate_use_days,extrapolate_length){
  length_xx <- length(xx)
  pick_end <- (length_xx - min(extrapolate_use_days,length_xx) + 1):length_xx
  pick_xx <- xx[pick_end] # Extract data from window

  # Fit simple linear regression
  
  log_yy <- log(pick_xx); length_yy <- length(log_yy)
  model_1 <- lm(log_yy ~ pick_end)
  
  param_1 <- model_1$coefficients[1] %>% as.numeric()
  param_2 <- model_1$coefficients[2] %>% as.numeric()
  
  # Output extrapolated values
  extrapolate_vals <- exp(param_1 + ((length_xx+1):(length_xx+extrapolate_length))*param_2)
  extrapolate_vals %>% round() # make integers
}

# Output new values


# Add entries for book-keeping
extra_entries <- (matrix(NA,nrow=extrapolate_length,ncol=ncol(all_dat)) %>% as_tibble()); names(extra_entries) <- names(all_dat)
all_dat2 <- rbind(all_dat,extra_entries)
final_pick <- (nrow(all_dat)+1):(nrow(all_dat)+extrapolate_length)
all_dat2[final_pick,]$date <- max(all_dat$date) + c(1:extrapolate_length)

# Run incidence calculations
all_dat2[final_pick,]$new_cases_china     <- extrapolate_data(all_dat$new_cases_china,extrapolate_use_days,extrapolate_length)
all_dat2[final_pick,]$new_deaths_china    <- extrapolate_data(all_dat$new_deaths_china,extrapolate_use_days,extrapolate_length)
all_dat2[final_pick,]$new_sev_cases_china <- extrapolate_data(all_dat$new_sev_cases_china,extrapolate_use_days,extrapolate_length)
all_dat2[final_pick,]$new_cases_outside   <- extrapolate_data(all_dat$new_cases_outside,extrapolate_use_days,extrapolate_length)
#all_dat2[final_pick,]$new_deaths_outside  <- extrapolate_data(all_dat$new_deaths_outside,extrapolate_use_days,extrapolate_length)

# Calculate cumulative
all_dat2[final_pick,]$cumulative_cases_china     <- tail(all_dat$cumulative_cases_china,1) + cumsum(all_dat2[final_pick,]$new_cases_china)
all_dat2[final_pick,]$cumulative_deaths_china    <- tail(all_dat$cumulative_deaths_china,1) + cumsum(all_dat2[final_pick,]$new_deaths_china)
all_dat2[final_pick,]$cumulative_sev_cases_china <- tail(all_dat$cumulative_sev_cases_china,1) + cumsum(all_dat2[final_pick,]$new_sev_cases_china)
all_dat2[final_pick,]$cumulative_cases_outside   <- tail(all_dat$cumulative_cases_outside,1) + cumsum(all_dat2[final_pick,]$new_cases_outside )
all_dat2[final_pick,]$cumulative_deaths_outside  <- tail(all_dat$cumulative_deaths_outside,1) + cumsum(all_dat2[final_pick,]$new_deaths_outside)



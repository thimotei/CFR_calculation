# Define CFR function -----------------------------------------------------

# Function to work out correction CFR
scale_cfr_temporal <- function(data_1_in, delay_fun = hospitalisation_to_death_truncated){
  
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  cumulative_known_t <- NULL # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- c(cumulative_known_t,known_i) # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- (death_incidence/cumulative_known_t) %>% pmin(.,1)
  
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), deaths = death_incidence,
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}

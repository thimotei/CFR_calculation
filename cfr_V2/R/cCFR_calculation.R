library(dplyr)
library(tidyverse)
library(magrittr)

# Format CI text ------------------------------------------------

c.text <- function(x = NULL, sigF = 2){
  bp1=signif(c(median(x), quantile(x, 0.025), 
               quantile(x, 0.975)), sigF)
  paste(bp1[1], " (", bp1[2], "-", bp1[3], ")",sep = "")
}

c.input.text <- function(bp1 = NULL, sigF = 2){
  bp1 <- signif(100 * bp1, sigF)
  paste(bp1[1], " (", bp1[2], "-", bp1[3], ")",sep = "")
}

# Calculation binomial CIs ------------------------------------------------
bin_conf <- function(x, n){
  htest <- binom.test(x, n)
  h_out <- c(x / n, htest$conf.int[1], htest$conf.int[2])
  h_out
}


# function to calculate Equation 6 from Nishiura et al. (2009)
# "Early Epidemiological Assessment..."

scale_cfr <- function(data_1_in = NULL, delay_fun = NULL){
  
  # DEBUG   data_1_in <- data_1[1:tt,]
  
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  # cumulative cases with known outcome at time tt
  cumulative_known_t <- 0 
  
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    # number of cases with known outcome at time ii
    known_i <- 0 
    
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj] * delay_fun(jj))
      known_i <- known_i + known_jj
    }
    # Tally cumulative known
    cumulative_known_t <- cumulative_known_t + known_i 
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  
  out <- c(b_tt, p_tt, sum(death_incidence),
    round(cumulative_known_t), sum(case_incidence))
  
  return(out)
}


# function to loop over all data and calcuate the corrected CFR over time
output_cfr_timeseries <- function(main_data_in = NULL, delay_fun = NULL) {
  
  # Load data and omit initial missing entries
  data_1 <- main_data_in[!is.na(main_data_in$new_cases),] 
  data_1 <- data_1 %>% 
    # scale up based on exported case estimates
    # Not clear to me what scaled_reporting is here? 
    mutate(new_cases = scaled_reporting * new_cases,
           new_deaths = new_deaths)
  
  # Calculate adjusted CFR
  store_cfr <- NULL
  for(tt in 1:nrow(data_1))
  {
    store_cfr <- rbind(store_cfr,scale_cfr(data_1[1:tt,], delay_fun))
  }
  
  store_cfr <- as_tibble(store_cfr)
  names(store_cfr) <- c("naive","cCFR","deaths","known_outcomes","cases")
  
  return(store_cfr)
}


  # Calculate final CI and output
output_estimates <- function(main_data_in = NULL, delay_fun = NULL) {
  
  ## This code is a complete c + p from above - suggest a function
  # Load data and omit initial missing entries
  data_1 <- main_data_in[!is.na(main_data_in$new_cases),] 
  data_1 <- data_1 %>% 
    # scale up based on exported case estimates
    mutate(new_cases = scaled_reporting * new_cases,
           new_deaths = new_deaths)
  
  # Calculate corrected CFR
  store_cfr <- NULL
  for(tt in 1:nrow(data_1))
  {
    ## If you are having any speed problems rbind is known to be slow.
    ## Suggest using data.table::rbindlist or dplyr::bind_rows
    store_cfr <- rbind(store_cfr,scale_cfr(data_1[1:tt,], delay_fun))
  }
  
  # store in tibble
  store_cfr <- as_tibble(store_cfr); 
  names(store_cfr) <- c("naive","cCFR","deaths","known_outcomes","cases")
  
  tt_max <- nrow(data_1)
  
  # calculate confidence intervals on estimates
  CI_nCFR_raw <- c.input.text(bin_conf(data_1[tt_max,]$deaths, 
                                       data_1[tt_max,]$cases))
  CI_nCFR <- c.input.text(bin_conf(data_1[tt_max,]$deaths, 
                                   round(scaled_reporting * data_1[tt_max,]$cases)))
  CI_cCFR <- c.input.text(bin_conf(store_cfr$deaths[tt_max], 
                                   round(scaled_reporting * (store_cfr$known_outcomes[tt_max]))))
  
  
  output_estimate_data <- rbind(c("Naive CFR (raw data)", CI_nCFR_raw),
                                c("Naive CFR (scaled data)", CI_nCFR),
                                c("Adjusted CFR (scaled data)", CI_cCFR))
  
  output_estimate_data <- as_tibble(output_estimate_data)
  names(output_estimate_data) <- c("Data/method","Estimate")
  
  #write_csv(output_estimate_data,paste0("~/cfr_table_scale_",scaled_reporting,"_",tail(data_1,1)$date,".csv"))
  
  return(output_estimate_data)
  
}

# 

calculate_CIs_nCFR <- function(data_1)
{
  
  CI_nCFR_raw_mid<- NA
  CI_nCFR_raw_low<- NA
  CI_nCFR_raw_high<- NA
  
  for(i in 1:nrow(data_1))
  {
    
    CI_nCFR_raw_mid[i] <- signif(bin_conf(data_1[i,]$deaths, 
                                          data_1[i,]$cases)[1]*100, 3)
    CI_nCFR_raw_low[i] <- signif(bin_conf(data_1[i,]$deaths, 
                                          data_1[i,]$cases)[2]*100, 3)
    CI_nCFR_raw_high[i] <- signif(bin_conf(data_1[i,]$deaths, 
                                           data_1[i,]$cases)[3]*100)

  }
  
  output_CIs <-  data.frame(date = data_1$date, 
                            ci_mid = CI_nCFR_raw_mid,
                            ci_low = CI_nCFR_raw_low, 
                            ci_high = CI_nCFR_raw_high)
  return(output_CIs)
}


calculate_CIs_cCFR <- function(data_1)
{
  
  CI_cCFR_raw_mid <- NA
  CI_cCFR_raw_low <- NA
  CI_cCFR_raw_high <- NA
  
   for(i in 1:nrow(data_1))
   {
     
     CI_cCFR_raw_mid[i] <- signif(bin_conf(data_1[i,]$deaths, 
                                           data_1[i,]$known_outcomes)[1]*100, 2)
     CI_cCFR_raw_low[i] <- signif(bin_conf(data_1[i,]$deaths,
                                           data_1[i,]$known_outcomes)[2]*100, 2)
     CI_cCFR_raw_high[i] <- signif(bin_conf(data_1[i,]$deaths, 
                                            data_1[i,]$known_outcomes)[3]*100, 2)
     
   }
   
   output_CIs <-  data.frame(date = data_1$date,
                             ci_mid = CI_cCFR_raw_mid,
                             ci_low = CI_cCFR_raw_low, 
                             ci_high = CI_cCFR_raw_high)
   return(output_CIs)
   
}



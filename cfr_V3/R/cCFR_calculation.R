library(dplyr)
library(tidyverse)
library(magrittr)

# Format CI text ------------------------------------------------

c.text<-function(x,sigF=3){
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

c.input.text<-function(bp1,sigF=3){
  bp1 <- signif(100*bp1,sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

# Calculation binomial CIs ------------------------------------------------
bin_conf <- function(x,n){
  htest <- binom.test(x,n)
  h_out <- c(x/n, htest$conf.int[1], htest$conf.int[2])
  h_out
}


# function to calculate Equation 6 from Nishiura et al. (2009)
# "Early Epidemiological Assessment..."

scale_cfr <- function(data_1_in, delay_fun){
  
  # DEBUG   data_1_in <- data_1[1:tt,]
  
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    
    known_i <- 0 # number of cases with known outcome at time ii
    
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  
  c(b_tt, p_tt, sum(death_incidence), round(cumulative_known_t), sum(case_incidence))
  
}


# function to loop over all data and calcuate the corrected CFR over time
output_cfr_timeseries <- function(main_data_in, delay_fun){
  
  # Load data and omit initial missing entries
  data_1 <- main_data_in[!is.na(main_data_in$new_cases),] 
  data_1 <- data_1 %>% mutate(new_cases = scaled_reporting*new_cases,
                              new_deaths = new_deaths) # scale up based on exported case estimates
  
  # Calculate adjusted CFR
  store_cfr <- NULL
  for(tt in 1:nrow(data_1))
  {
    store_cfr <- rbind(store_cfr,scale_cfr(data_1[1:tt,], delay_fun))
  }
  
  store_cfr <- as_tibble(store_cfr); names(store_cfr) <- c("naive","cCFR","deaths","known_outcomes","cases")
  
  return(store_cfr)
}


calculate_CIs_nCFR <- function(data_1)
{
  
  CI_nCFR_raw_mid<- NA
  CI_nCFR_raw_low<- NA
  CI_nCFR_raw_high<- NA
  
  for(i in 1:nrow(data_1))
  {
    
    CI_nCFR_raw_mid[i] <- signif(bin_conf(data_1[i,]$deaths,data_1[i,]$cases)[1]*100, 3)
    CI_nCFR_raw_low[i] <- signif(bin_conf(data_1[i,]$deaths,data_1[i,]$cases)[2]*100, 3)
    CI_nCFR_raw_high[i] <- signif(bin_conf(data_1[i,]$deaths,data_1[i,]$cases)[3]*100)

  }
  
  output_CIs <-  data.frame(date = data_1$date, 
                            ci_mid = CI_nCFR_raw_mid,
                            ci_low = CI_nCFR_raw_low, 
                            ci_high = CI_nCFR_raw_high)
  return(output_CIs)
}


calculate_CIs_cCFR <- function(data_1, delay_dist)
{
  
  CI_cCFR_raw_mid <- NA
  CI_cCFR_raw_low <- NA
  CI_cCFR_raw_high <- NA
    
    for(i in 1:nrow(data_1))
    {
      if(data_1[i,]$known_outcomes >= data_1[i,]$deaths)
      {
        CI_cCFR_raw_mid[i]  <- signif(bin_conf(data_1[i,]$deaths, data_1[i,]$known_outcomes)[1]*100, 3)
        CI_cCFR_raw_low[i]  <- signif(bin_conf(data_1[i,]$deaths, data_1[i,]$known_outcomes)[2]*100, 3)
        CI_cCFR_raw_high[i] <- signif(bin_conf(data_1[i,]$deaths, data_1[i,]$known_outcomes)[3]*100, 3)
      }
      else if(data_1[i,]$known_outcomes < data_1[i,]$deaths)
      {
        CI_cCFR_raw_mid[i]  <- signif(bin_conf(data_1[i,]$known_outcomes, data_1[i,]$deaths)[1]*100, 3)
        CI_cCFR_raw_low[i]  <- signif(bin_conf(data_1[i,]$known_outcomes, data_1[i,]$deaths)[2]*100, 3)
        CI_cCFR_raw_high[i] <- signif(bin_conf(data_1[i,]$known_outcomes, data_1[i,]$deaths)[3]*100, 3)
      }
   }
   
   output_CIs <-  data.frame(date = data_1$date,
                             ci_mid = CI_cCFR_raw_mid,
                             ci_low = CI_cCFR_raw_low, 
                             ci_high = CI_cCFR_raw_high)
   return(output_CIs)
   
}

nCFR_with_CIs_main_time_series <- function(data_1, delay_dist)
{
  
  dateRange <- seq(as.Date(min(data_1$date)), as.Date(max(data_1$date)), 1)
  
  CFRVariablename1 <- as.name(paste0("cCFRRealisticTime_", deparse(substitute(data_1))))
  CFRVariablename2 <- as.name(paste0("cCFRRealisticTimeConstrained_", deparse(substitute(data_1))))
  CFRVariablename3 <- as.name(paste0("CFRCIs_", deparse(substitute(data_1))))
  
  output_cfr_timeseries(data_1, delay_dist)  -> CFRVariablename1
  cbind(date = dateRange, CFRVariablename1) -> CFRVariablename1
  subset(CFRVariablename1, deaths > 0 & known_outcomes > 0) -> CFRVariablename2
  calculate_CIs_nCFR(CFRVariablename2) -> CFRVariablename3
  
  CFRVariablename3$date <- as.Date(CFRVariablename3$date)
  
  return(CFRVariablename3)
  
}


cCFR_with_CIs_main_time_series <- function(data_1, delay_dist)
{
  
  dateRange <- seq(as.Date(min(data_1$date)), as.Date(max(data_1$date)), 1)
  
  CFRVariablename1 <- as.name(paste0("cCFRRealisticTime_", deparse(substitute(data_1))))
  CFRVariablename2 <- as.name(paste0("cCFRRealisticTimeConstrained_", deparse(substitute(data_1))))
  CFRVariablename3 <- as.name(paste0("CFRCIs_", deparse(substitute(data_1))))
  
  output_cfr_timeseries(data_1, delay_dist)  -> CFRVariablename1
  cbind(date = dateRange, CFRVariablename1) -> CFRVariablename1
  subset(CFRVariablename1, deaths > 0 & known_outcomes > 0) -> CFRVariablename2
  calculate_CIs_cCFR(CFRVariablename2) -> CFRVariablename3
  
  CFRVariablename3$date <- as.Date(CFRVariablename3$date)
  
  return(CFRVariablename3)
  
}

computenCFRTimeSeries <- function(data_in)
{
  
  data_in_2 <- pad(data_in)
  data_in_2 <- data_in_2   %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) 
  data_in_2 <- data_in_2 %>% mutate_if(is.factor, ~replace(., is.na(.), data_in$country[1])) 
  
  currentnCFRTimeSeries <- nCFR_with_CIs_main_time_series(data_in_2, hospitalisation_to_death_truncated)
  
}

computecCFRTimeSeries <- function(data_in)
{
  
  data_in_2 <- pad(data_in)
  data_in_2 <- data_in_2   %>% mutate_if(is.integer, ~replace(., is.na(.), 0)) 
  data_in_2 <- data_in_2 %>% mutate_if(is.factor, ~replace(., is.na(.), data_in$country[1])) 
  
  currentcCFRTimeSeries <- cCFR_with_CIs_main_time_series(data_in_2, hospitalisation_to_death_truncated)
}



scale_by_age_distribution_cases <- function(data_1, sf_data)
{
  
  
  data_1_scaled_cases <- matrix(NA, nrow = length(data_1$new_cases), ncol = length(sf_data$case_sf))
  for(i in 1:length(sf_data$case_sf))
  {
    data_1_scaled_cases[,i] <- data_1$new_cases*sf_data$case_sf[i]
  }
  
  return(data_1_scaled_cases)
}


scale_by_age_distribution_deaths <- function(data_1, sf_data)
{
  
  
  data_1_scaled_deaths <- matrix(NA, nrow = length(data_1$new_deaths), ncol = length(sf_data$death_sf))
  for(i in 1:length(sf_data$death_sf))
  {
    data_1_scaled_deaths[,i] <- data_1$new_deaths*sf_data$death_sf[i]
  }
  
  return(data_1_scaled_deaths)
}
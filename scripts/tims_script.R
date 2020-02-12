library(dplyr)
library(magrittr)

######### read in data ##############

caseDataRaw <- readRDS("~/Dropbox/nCov-2019/data_sources/case_data/hubei_confirmed.rds")
deathDataRaw <- readRDS("~/Dropbox/nCov-2019/data_sources/case_data/all_death_prf.rds")
load("~/repos/2020-ncov/stoch_model/outputs/bootstrap_fit_1.RData")

######### format data ################


cumulativeCaseTimeSeriesWuhan <- data.frame(caseDataRaw$date[caseDataRaw$CNTY_CODE==420100], caseDataRaw$total_case[caseDataRaw$CNTY_CODE==420100])
cumulativeDeathTimeSeriesWuhan <- data.frame(deathData$date[deathData$CNTY_CODE==420100],  Data$fatality[deathData$CNTY_CODE==420100])
cumulativeDeathTimeSeriesWuhanTime <- subset(cumulativeDeathTimeSeriesWuhan, date >= cumulativeCaseTimeSeriesWuhan[1,1])

colnames(cumulativeCaseTimeSeriesWuhan)[1] <-"date"
colnames(cumulativeCaseTimeSeriesWuhan)[2] <-"case_incidence"

colnames(cumulativeDeathTimeSeriesWuhan)[1] <-"date"
colnames(cumulativeDeathTimeSeriesWuhan)[2] <-"death_incidence"

# adding missing row of case data

missingRow <- data.frame(date = "2020-01-20", case_incidence = 291)
tmp <- rbind(cumulativeCaseTimeSeriesWuhan, missingRow)
cumulativeCaseTimeSeriesWuhan <- tmp[order(as.Date(tmp$date, format="%Y/%m/%d")),]


incidenceAtT <- diff(C_local_plot)
incidenceAtTDF <- data.frame(incidenceAtT)
cumulativeCaseTimeSeriesWuhanInferred <-rowMeans(incidenceAtTDF, na.rm = TRUE)

caseDeathIncidenceTogether <- data.frame(date = cumulativeDeathTimeSeriesWuhanTime$date,
                                         case_incidence = cumulativeCaseTimeSeriesWuhan$case_incidence,
                                         death_incidence =cumulativeDeathTimeSeriesWuhanTime$death_incidence)




######### define date range of interest ##

start_date <- as.Date("2019-11-22") # first case
end_date <- as.Date("2020-03-01") # period to forecast ahead
end_date_minus_1 <- as.Date("2020-02-29") # period to forecast ahead
t_period <- as.numeric(end_date-start_date) # for numerical use of the time difference
date_range <- seq(start_date,end_date_minus_1,1) # creating a vector the time range for plots etc

######### parameterising the delay distributions (taken from papers) ############

meanG <- 13.8 - 5.1 # subtract delay from onset-to-confirmation
scaleG <- 1
onset_to_death <- function(x){ dgamma(x, meanG/scaleG, scale = scaleG) }

meanH <- 9.1
scaleH <- 1
onset_to_hosp <- function(x){ dgamma(x, meanH/scaleH, scale = scaleH) }


########## computing the scaling factor for the corrected CFR


scale_cfr <- function(data_1_in,delay_fun){
  
  # DEBUG   data_1_in <- data_1[1:tt,]
  
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    
    known_i <- 0 # number of cases with known outcome at time ii
    
    for(jj in 0:(ii-1)){
      known_jj <- (data_1_in[ii-jj,]$case_incidence * delay_fun(jj) )
      known_i <- known_i + known_jj
    }
    known_i
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(data_1_in$death_incidence)/sum(data_1_in$case_incidence) 
  
  # nCFR estimator
  p_tt <- sum(data_1_in$death_incidence)/cumulative_known_t
  
  c(b_tt,p_tt,sum(data_1_in$death_incidence),round(cumulative_known_t),sum(data_1_in$case_incidence))
  
}

tmp2 <-  data.frame(new_cases = diff(caseDeathIncidenceTogether$case_incidence),new_deaths = diff(caseDeathIncidenceTogether$death_incidence))
allDataTogether <- cbind(caseDeathIncidenceTogether[2:28,1:3], tmp2)




output_estimates <- function(main_data_in){
  
  # Load data and omit initial missing entries
  data_1 <- main_data_in[!is.na(main_data_in$new_cases),] 
  data_1 <- data_1 %>% mutate(cases = scaled_reporting*new_cases,
                              deaths = new_deaths) # scale up based on exported case estimates
  

  # SPECIFY WHETHER CHINA OR OUTSIDE
  #data_1 <- main_data_in[!is.na(main_data_in$new_cases_china),] 
  #data_1 <- data_1 %>% mutate(cases = scaled_reporting*new_cases_china,
  #                            deaths = new_deaths_china) # scale up based on exported case estimates
  
  
  # - - 
  # Estimate fatality risk
  
  # Check have enough of distribution
  distn_needed <- sum(cumsum(onset_to_death(0:30))<=0.95) # make sure have at 95% of the cumulative distn
  
  # Calculate adjusted CFR
  mov_window <- distn_needed
  store_cfr <- NULL
  for(tt in 1:nrow(data_1)){
    store_cfr <- rbind(store_cfr,scale_cfr(data_1[1:tt,],delay_fun = onset_to_death))
  }

  
  
  store_cfr <- as_tibble(store_cfr); names(store_cfr) <- c("naive","cCFR","deaths","known_outcomes","cases")
  
  #Estimate severity risk
  
  # Check have enough of distribution
  distn_needed <- sum(cumsum(onset_to_hosp(0:30))<=0.95) # make sure have at 95% of the cumulative distn
  
  # Calculate adjusted severity risk
  mov_window <- distn_needed
  #data_2 <- data_1; 
  #data_2 <- data_2 %>% mutate(cases = new_cases_china,
  #                            deaths = new_sev_cases_china) # scale up based on exported case estimates
  #data_2 <- data_2[!is.na(data_2$new_sev_cases_china),]# omit NAs
  
  #store_sev <- NULL
  #for(tt in 1:nrow(data_2)){
  #  store_sev <- rbind(store_sev,scale_cfr(data_2[1:tt,],delay_fun = onset_to_hosp))
  #}
  #store_sev <- as_tibble(store_sev); names(store_sev) <- c("naive","cCFR","severe","known_outcomes","cases")

  
  # - - - 
  # Calculate final CI and output
  tt_max <- nrow(data_1)
  
  CI_nCFR_raw <- c.input.text(bin_conf(data_1[tt_max,]$death_incidence,data_1[tt_max,]$case_incidence))
  CI_nCFR <- c.input.text(bin_conf(data_1[tt_max,]$deaths,scaled_reporting*data_1[tt_max,]$case_incidence))
  CI_cCFR <- c.input.text(bin_conf(store_cfr[tt_max,]$deaths,store_cfr[tt_max,]$known_outcomes))
  

  
  output_estimate_data <- rbind(c("Naive CFR (raw data)",CI_nCFR_raw),
                            c("Naive CFR (scaled data)",CI_nCFR),
                            c("Adjusted CFR (scaled data)",CI_cCFR))
  output_estimate_data <- as_tibble(output_estimate_data)
  names(output_estimate_data) <- c("Data/method","Estimate")
  
  
  
  write_csv(output_estimate_data,paste0("plots/cfr_table_scale_",scaled_reporting,"_extrapo_",extrapolate_use_days,"_",tail(data_1,1)$date,".csv"))
  
}

mean_onset_to_death <- 22.3

death_future_date <- as.Date(Sys.Date()) + mean_onset_to_death
death_past_date <- as.Date(end_date) - mean_onset_to_death



  
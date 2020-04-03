# Code to estimate reporting

# Sanbox to look at temporal variation in reporting


# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(mgcv)

# Set paths
setwd("~/Documents/lshtm/github repos/CFR_calculation/global_estimates/")
if(grepl(Sys.info()["user"], pattern = "^adamkuchars(ki)?$")){setwd("~/Documents/GitHub/CFR_calculation/global_estimates/")}
if(Sys.info()["user"] == 'hamishgibbs'){setwd("~/Documents/Covid-19/CFR_calculation/global_estimates")}

# Set parameters
zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
#cCFRIQRRange <- c(1.3, 1.4)


# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, muHDT, sigmaHDT) - plnorm(x, muHDT, sigmaHDT)
}


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
  
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}


# Load data -----------------------------------------------------

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read_csv(tf)


allDatDesc <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(!country %in% c("CANADA", "Cases_on_an_international_conveyance_Japan"))

# Do analysis - remove all countries with cumsum deaths < 15
allTogetherCleanA <- allDatDesc %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::mutate(cum_sum_deaths = cumsum(new_deaths)) %>%
  dplyr::filter(cum_sum_deaths >= 15) %>%
  dplyr::select(-cum_sum_deaths) %>% 
  ungroup()


# Plot rough reporting over time -----------------------------------------

#countrypick <- "Denmark"
#source file into this script?
#could do cumsum on whole dataset

get_plot_data <- function(country_name, data = allTogetherCleanA){
  
  true_cfr <- 1.4/100
  
  country_data <- data %>% 
    filter(country == country_name) %>% 
    mutate(date = date - zmeanHDT)
    
  cfr <- scale_cfr_temporal(country_data) %>% 
    as_tibble() %>% 
    mutate(reporting_estimate = true_cfr/cCFR) %>% 
    mutate(reporting_estimate = pmin(reporting_estimate, 1))
  
  return(cfr)
  
}

get_plot_data(country_name = 'Portugal') %>% pull(cCFR)

countrypickList <- c("China","United_Kingdom","Germany","South_Korea",
                     "United_States_of_America","Denmark","Spain","Portugal")

par(mfrow=c(2,4),mar=c(3,3,1,1),mgp=c(2,0.6,0))

for(ii in 1:length(countrypickList)){
  
  countrypick <- countrypickList[ii]
  
  data_1_in_Pick <- allTogetherCleanA %>% filter(country==countrypick)
  true_cfr <- 1.4/100
  cum_sum_d <- cumsum(data_1_in_Pick$new_deaths)
  pick_period <- (cum_sum_d>=15)
  
  out_cfr <- scale_cfr_temporal(data_1_in_Pick)
  reporting_estimate <- (true_cfr/out_cfr$cCFR) %>% pmin(.,1)
  reporting_estimate[!pick_period] <- NA # remove before deaths
  xx_date <- data_1_in_Pick$date-zmeanHDT
  
  # Rough plot & set colours
  plot(xx_date[pick_period],reporting_estimate[pick_period],ylim=c(0,1),ylab="proportion of cases reported",xlab="",main=countrypick)
  
  col_def <-   list(col1=rgb(1,0,0),col2=rgb(0.8,0.6,0),col3=rgb(0,0.8,0.8),col4=rgb(0.1,0.4,0.1),col5=rgb(1,0.4,1),col6=rgb(0.2,0,0.8),"dark grey")
  col_def_F <- list(col1=rgb(1,0,0,0.2),col2=rgb(0.8,0.6,0,0.5),col3=rgb(0,0.8,0.8,0.5),col5=rgb(0.1,0.4,0.1,0.5),col6=rgb(1,0.4,1,0.5),col7=rgb(0.2,0,0.8,0.5),"grey")
  
  # Collate data and fit GAM model
  data_fit <- cbind(xx_date,reporting_estimate)
  data_fit <- as_tibble(data_fit); names(data_fit) <- c("date","report")
  
  modelB.P <- gam(report ~ s(date) , data = data_fit,family = "gaussian")
  
  # Predict data
  xx_pred <- xx_date[pick_period]
  preds <- predict(modelB.P, newdata = list(date=as.numeric(xx_pred)), type = "link", se.fit = TRUE)
  critval <- 1.96; upperCI <- preds$fit + (critval * preds$se.fit); lowerCI <- preds$fit - (critval * preds$se.fit)
  fit <- preds$fit
  fitPlotF <- modelB.P$family$linkinv(fit); CI1plotF <- modelB.P$family$linkinv(upperCI);  CI2plotF <- modelB.P$family$linkinv(lowerCI)
  
  polygon(c(xx_pred,rev(xx_pred)),c(CI1plotF,rev(CI2plotF)),col=col_def_F[[1]],lty=0)
  lines(xx_pred, fitPlotF ,col=col_def[[1]],lwd=2)
  
  # Estimate true symptomatic cases
  
  # Align case data with reporting
  # estimate_cases <- head(data_1_in_Pick$new_cases,-zmeanHDT)/tail(reporting_estimate,-zmeanHDT) 
  # date_match <- head(data_1_in_Pick$date,-zmeanHDT)
  # 
  #plot(date_match,estimate_cases,ylab="cases",xlab="",log="y",main=countrypick)
  #lines(data_1_in_Pick$date,data_1_in_Pick$new_cases,col="blue")
}
  
dev.copy(png,paste("outputs/calc_1.png",sep=""),units="cm",width=20,height=12,res=150)
dev.off()
  
  

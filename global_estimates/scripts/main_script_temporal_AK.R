# Code to estimate reporting

# Sanbox to look at temporal variation in reporting


# Set up paths and parameters ---------------------------------------------

# Load libraries
library(tidyverse)
library(padr)
library(mgcv)

# Set paths
setwd("~/Documents/lshtm/github repos/CFR_calculation/global_estimates/")
if(grepl(Sys.info()["user"], pattern = "^a(dam)kuchars(ki)?$")){setwd("~/Documents/GitHub/CFR_calculation/global_estimates/")}

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
             cum_known_t = cumulative_known_t, total_cases = sum(case_incidence))
}


# Load data -----------------------------------------------------

httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
allDat <- read.csv(tf)


allDatDesc <- allDat %>% 
  dplyr::arrange(countriesAndTerritories, dateRep) %>% 
  dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
  dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
  dplyr::select(date, country, new_cases, new_deaths) %>%
  dplyr::filter(country != "CANADA", 
                country != "Cases_on_an_international_conveyance_Japan")
# Do analysis
allTogetherCleanA <- allDatDesc %>%
  dplyr::group_by(country) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths) 

# Import reporting data
testing_data <- read_csv("~/Dropbox/LSHTM/2020_nCoV_main_db/2020_03_CFR_estimates/reporting_proportion/testing_data/covid-testing-12-Apr-all-observations.csv")
testing_data1 <- read_csv("~/Dropbox/LSHTM/2020_nCoV_main_db/2020_03_CFR_estimates/reporting_proportion/testing_data/full-list-covid-19-tests-per-day.csv")
# 
# d0 <- testing_data %>% filter(Entity=="United Kingdom - people tested")
# d1 <- testing_data1 %>% filter(Entity=="United Kingdom")
# 

# Plot rough reporting over time -----------------------------------------

#countrypick <- "Denmark"
countrypickList <- c("China","United_Kingdom","Germany","South_Korea",
                     "United_States_of_America","Denmark","Spain","Portugal")

par(mfrow=c(2,4),mar=c(3,3,1,1),mgp=c(2,0.6,0))

for(ii in 1:length(countrypickList)){
  
  countrypick <- countrypickList[ii]
  
  data_1_in_Pick <- allTogetherCleanA %>% filter(country==countrypick)
  true_cfr <- 1.4/100
  cum_sum_d <- cumsum(data_1_in_Pick$new_deaths)
  pick_period <- (cum_sum_d>=10)
  
  write_csv(data_1_in_Pick,"cases_korea.csv")
  
  out_cfr <- scale_cfr_temporal(data_1_in_Pick)
  reporting_estimate <- (true_cfr/out_cfr$cCFR) %>% pmin(.,1)
  #reporting_estimate[!pick_period] <- NA # remove before deaths
  
  cases_known <- out_cfr$cum_known_t
  
  xx_date <- data_1_in_Pick$date-zmeanHDT
  
  # Rough plot & set colours
  plot(xx_date[pick_period],reporting_estimate[pick_period],ylim=c(0,1),ylab="proportion of cases reported",xlab="",main=countrypick)
  
  col_def <-   list(col1=rgb(1,0,0),col2=rgb(0.8,0.6,0),col3=rgb(0,0.8,0.8),col4=rgb(0.1,0.4,0.1),col5=rgb(1,0.4,1),col6=rgb(0.2,0,0.8),"dark grey")
  col_def_F <- list(col1=rgb(1,0,0,0.2),col2=rgb(0.8,0.6,0,0.5),col3=rgb(0,0.8,0.8,0.5),col5=rgb(0.1,0.4,0.1,0.5),col6=rgb(1,0.4,1,0.5),col7=rgb(0.2,0,0.8,0.5),"grey")
  
  # Collate data and fit GAM model
  data_fit <- cbind(data_1_in_Pick$date,xx_date,reporting_estimate,cases_known,data_1_in_Pick$new_deaths,data_1_in_Pick$new_cases)
  data_fit <- as_tibble(data_fit); names(data_fit) <- c("date_raw","date","report","cases_known","deaths","cases")
  data_fit <- data_fit %>% mutate(log_offset = log(cases_known)*0.0138)
  
  data_fit <- data_fit[!is.na(data_fit$report),]
  
  # log_offset <- log(cases_known * 0.0138)
  # m <- mgcv::gam(deaths ~ s(date) + offset(log_offset),
  #                family = stats::poisson)
  # 
  
  modelB.P <- gam(deaths ~ s(date) + offset(log_offset) , data = data_fit,family = "gaussian")

  # Predict data
  xx_pred <- xx_date
  preds <- predict(modelB.P, newdata = list(date=as.numeric(xx_pred),log_offset=rep(0,length(xx_pred))), type = "link", se.fit = TRUE)
  critval <- 1.96; upperCI <- preds$fit + (critval * preds$se.fit); lowerCI <- preds$fit - (critval * preds$se.fit)
  fit <- preds$fit
  fitPlotF <- modelB.P$family$linkinv(fit); CI1plotF <- modelB.P$family$linkinv(upperCI);  CI2plotF <- modelB.P$family$linkinv(lowerCI)
  
  polygon(c(xx_pred,rev(xx_pred)),c(CI1plotF,rev(CI2plotF)),col=col_def_F[[1]],lty=0)
  lines(xx_pred, fitPlotF ,col=col_def[[1]],lwd=2)
  
  # Testing analysis
  
  testing_data1 <- testing_data %>% filter(Entity == "South Korea - cases tested"  )
  
  # Match data
  n_pick <- match(data_fit$date,testing_data1$Date);
  testing_data2 <- testing_data1[n_pick,]
  
  data_fit2 <- data_fit %>% add_column(tests = testing_data2$`Daily change in cumulative total`)
  data_fit2 <- data_fit2[!is.na(data_fit$report) & (cumsum(data_fit2$deaths)>10),]
  
  modelB.P <- glm(report ~ cases + tests, data = data_fit2,family = "logit")
  summary(modelB.P)
  
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


# Plot cases
#countrypick <- "China"; ylim1 <- c(0,4000); ylim2 <- c(0,160)
countrypick <- "South_Korea"; ylim1 <- c(0,1000); ylim2 <- c(0,10)
#countrypick <- "Austria"; ylim1 <- c(0,900); ylim2 <- c(0,25)
countrypick <- "Spain"; ylim1 <- c(0,9000); ylim2 <- c(0,1e3)
countrypick <- "Italy"; ylim1 <- c(0,7e3); ylim2 <- c(0,1e3)
countrypick <- "Germany"; ylim1 <- c(0,7e3); ylim2 <- c(0,250)

data_1_in_Pick <- allTogetherCleanA %>% filter(country==countrypick)
#plot(data_1_in_Pick$date,data_1_in_Pick$new_cases)
data_1_in_Pick[data_1_in_Pick$new_cases>1e4,"new_cases"] <- 3000

library(forecast)

par(mfrow=c(1,1),mar=c(3,3,1,3),mgp=c(2,0.7,0),las=0)
x_date <- as.Date(c("2020-01-15","2020-04-11"))
plot(data_1_in_Pick$date,ma(data_1_in_Pick$new_cases,order=5),main=countrypick,xlab="",yaxs="i",ylim=ylim1,ylab="cases",xlim=x_date,type="l")

#lines(as.Date(c("2020-01-23","2020-01-23")),c(0,1e4),lty=2)

par(new=TRUE)
plot(data_1_in_Pick$date,ma(data_1_in_Pick$new_deaths,order=5),yaxs="i",ylim=ylim2,xaxt="n",yaxt="n",xlab="",ylab="",xlim=x_date,type="l",col="red")
axis(4,col="red",col.axis="red")
mtext("deaths", side=4, cex=1,line=1.8,col="red")

dev.copy(png,paste0("data_",countrypick,".png",sep=""),units="cm",width=15,height=10,res=150)
dev.off()




# Plot testing
par(mfrow=c(1,1),mar=c(3,3,1,1),mgp=c(2,0.6,0))
col_def <- list(col1=rgb(1,0,0),col2=rgb(0.8,0.6,0),col3=rgb(0,0.8,0.8),col4=rgb(0.1,0.4,0.1),col5=rgb(1,0.4,1),col6=rgb(0.2,0,0.8),"dark grey")

plot(as.Date("2020-01-01"),0,log="y",ylim=c(1,1e3),xlim=as.Date(c("2020-01-15","2020-04-12")),
     xlab="",ylab="tests performed per confirmed case",yaxt="n")
grid(ny = NULL, nx=NA, col = "lightgray")

yticks = c(10^seq(0,3,1))
axis(2, at = yticks,labels = yticks,col = "black") 

country_list <- c("Japan","United_Kingdom","Germany","South_Korea","Italy","United_States_of_America","Austria","Denmark","Italy")
test_list <- c("Japan - people tested","United Kingdom - people tested",
               "Germany - tests performed" ,"South Korea - cases tested","Italy - tests performed" ,
               "United States - specimens tested (CDC)")

for(ii in c(1:5)){
  countrypick1 <- country_list[ii]
  testingpick1 <- test_list[ii]

  t_data <- testing_data %>% filter(Entity == testingpick1 )
  c_data <- allTogetherCleanA %>% filter(country==countrypick1)
  c_data <- c_data %>% mutate(cum_cases = cumsum(new_cases))
  
  c_data2 <- c_data[match(t_data$Date,c_data$date),]
  
  c_data2 <- c_data2[!is.na(t_data$`Cumulative total`),]
  t_data <- t_data[!is.na(t_data$`Cumulative total`),]
  
  ukdat <- t_data

  lines(t_data$Date, t_data$`Cumulative total`/c_data2$cum_cases ,col=col_def[[ii]],lwd=2)
  #text(x=as.Date("2020-02-01"),y=exp(ii/3), labels=countrypick1,cex=0.9,col=col_def[[ii]])
}

# kdat
# ukdat

c_data <- allTogetherCleanA %>% filter(country=="Iceland")
# 
# 15141/2028

dev.copy(png,paste("~/Dropbox/LSHTM/2020_nCoV_main_db/2020_03_CFR_estimates/reporting_proportion/calc_1.png",sep=""),units="cm",width=15,height=10,res=150)
dev.off()

data_1_in_Pick <- allTogetherCleanA %>% filter(country=="United_Kingdom")
data_1_in_Pick <- allTogetherCleanA %>% filter(country=="China")

# plot(data_1_in_Pick$new_cases,type="l")
# plot(data_1_in_Pick$new_deaths,type="l")

# plot(as.Date("2020-01-01"),0,log="y",ylim=c(1,3e4),xlim=as.Date(c("2020-01-15","2020-04-07")),
#      xlab="",ylab="new cases",yaxt="n")
# 
# yticks = c(10^seq(0,5,1))
# axis(2, at = yticks,labels = yticks,col = "black") 
# 
# country_list <- c("Japan","United_Kingdom","Germany","South_Korea","Austria","Denmark","Italy")
# test_list <- c("Japan - people tested","United Kingdom - people tested",
#                "Germany - samples tested","South Korea - cases tested",
#                "Austria - units unclear","Denmark - people tested","Italy - units unclear" )
# 
# for(ii in c(1:4)){
#   countrypick1 <- country_list[ii]
#   testingpick1 <- test_list[ii]
#   
#   t_data <- testing_data %>% filter(Entity == testingpick1 )
#   c_data <- allTogetherCleanA %>% filter(country==countrypick1)
#   c_data <- c_data %>% mutate(cum_cases = cumsum(new_cases))
#   
#   c_data2 <- c_data[match(t_data$Date,c_data$date),]
#   
#   lines(c_data$date,c_data$new_cases ,col=col_def[[ii]],lwd=2)
#   text(x=as.Date("2020-02-01"),y=exp(ii/3), labels=countrypick1,cex=0.9,col=col_def[[ii]])
# }

pp_t <- 1-(1-0.07)^9
pp <- (9/14)/9

0.5*(5*pp*4+(4+3+2+1)*pp)/5

0.5*1*(1/14)


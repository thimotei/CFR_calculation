library(dplyr)
library(padr)

zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x)
{
 dlnorm(x, muHDT, sigmaHDT)
}


# Function to work out correction CFR
scale_cfr <- function(data_1_in, delay_fun){
 
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
 
 data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
            cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
 
}

# Get data
allDat <- NCoVUtils::get_ecdc_cases()

allDatDesc <- allDat %>% 
 dplyr::arrange(country, date) %>% 
 dplyr::mutate(date = lubridate::ymd(date)) %>% 
 dplyr::rename(new_cases = cases, new_deaths = deaths) %>%
 dplyr::select(date, country, new_cases, new_deaths) %>%
 dplyr::filter(country != "CANADA", 
               country != "Cases_on_an_international_conveyance_Japan")


# Do analysis
allTogetherClean2 <- allDatDesc %>%
 dplyr::group_by(country) %>%
 padr::pad() %>%
 dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
               new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
 dplyr::group_by(country) %>%
 dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
 dplyr::filter(cum_deaths > 0) %>%
 dplyr::select(-cum_deaths) %>%
 do(scale_cfr(., delay_fun = hospitalisation_to_death_truncated)) %>%
 dplyr::filter(cum_known_t > 0) %>%
 dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
               nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
               cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
               cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
               underreporting_estimate = 1 / (100*cCFR),
               lower = 0.3 / (100 * cCFR_LQ),
               upper = 3 / (100 * cCFR_UQ),
               quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
               quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2],
               bottom = 0.3 / (100 * quantile25),
               top = 3 / (100 * quantile75),
               confidence = dplyr::case_when(total_deaths < 10 ~ "Fewer than 10 deaths",
                                             total_deaths < 100 ~ "Fewer than 100 deaths, but more than 10",
                                             total_deaths >= 100 ~ "More than (or equal to) 100 deaths") %>% 
                factor(levels = c("More than (or equal to) 100 deaths", "Fewer than 100 deaths, but more than 10", "Fewer than 10 deaths"))) %>% 
 dplyr::ungroup()


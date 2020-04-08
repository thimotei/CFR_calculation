# working out under-reporting estimate and CIs
underReportingEstimates <- function(data, delay_fun){ 
  dplyr::group_by(data, country) %>%
    dplyr::do(scale_cfr(., delay_fun)) %>%
    dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
    dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                  nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                  cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                  cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                  underreporting_estimate = cCFRBaseline / (100*cCFR),
                  lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                  upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                  quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                  quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2]) %>% 
    dplyr::filter(total_deaths > 10)}
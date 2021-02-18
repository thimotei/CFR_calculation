library(reticulate)
use_condaenv('r-reticulate', required = TRUE)
library(greta)
library(greta.gp)

# if tensorflow is not installed to a virtual
# environment/conda environment, run this command
# to get the right versions of tensorflow installed

#greta::install_tensorflow(method = "conda",
                          #version = "1.14.0",
                          #extra_packages = "tensorflow-probability==0.7")

# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

# Set paths
setwd(here::here())

#source data processing and plotting scripts
source('global_estimates/temporal/R/jhu_data_import.R')
source('global_estimates/temporal/R/scale_cfr_temporal.R')
source('global_estimates/temporal/R/cases_known_convolution.R')
source('global_estimates/temporal/R/run_bayesian_model.R')

# setting baseline level CFR
cfr_baseline <- 1.4
cfr_range <- c(1.2, 1.7)

# Set parameters
mean <- 13
median <- 9.1

mu_cfr <- log(median)
sigma_cfr <- sqrt(2*(log(mean) - mu_cfr))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu_cfr, sigma_cfr) - plnorm(x, mu_cfr, sigma_cfr)
}

#--- Load and clean data using separate function
jhu_data <- case_death_timeseries_function()

#--- which country to run the full inference for
#--- inference is compute and memory heavy
#--- a HPC is used to run the inference for many countries/regions

iso_arg <- "GBR"
inference_data <- cases_known_convolution(iso_arg, jhu_data, cfr_baseline) %>%
    dplyr::filter(date > "2020-09-01")

#plot_data <- get_plot_data(country_name = plot_country_names, cfr_baseline = cfr_baseline)
prediction <- run_bayesian_model(inference_data,
                                 n_inducing = 5,
                                 cfr_baseline = cfr_baseline,
                                 cfr_range = cfr_range,
                                 cfr_trend = NULL,
                                 verbose = TRUE)

ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                          y = c(prediction$upper, rev(prediction$lower)))
  

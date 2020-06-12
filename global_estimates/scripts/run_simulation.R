# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

suppressPackageStartupMessages({
  require(readr)
  require(dplyr)
})

.args <- if (interactive()) c(
 "adjusted_cfr.csv", "something.csv", "../temporal/R/cfr_plot_theme.R",
  "../temporal/R/get_plot_data.R", "../temporal/R/plot_country.R",
  "../temporal/R/run_bayesian_model.R", "../temporal/R/scale_cfr_temporal.R", "AFG", "result_AFG.rds"
) else commandArgs(trailingOnly = TRUE)

# Load data -----------------------------------------------------
adjustedCFR <- read_csv(.args[1])
allDat <- read_csv(.args[2])
dataKey <- tail(.args, 2)[1]
someDat <- allDat %>% filter(country_code == dataKey)

target <- tail(.args, 1)

#source data processing and plotting scripts
srcs <- head(tail(.args, -2), -2)
sapply(srcs, source)

# setting baseline level CFR
CFRRow <- adjustedCFR %>% filter(iso3c == dataKey)
CFRBaseline <- CFRRow %>% select(cfr_mid) %>% as.numeric()
CFREstimateRange <- c(CFRRow %>% select(cfr_low), CFRRow %>% select(cfr_high)) %>% as.numeric()

# Set parameters
mean <- 13
median <- 9.1

mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}

plot_country_name <- someDat$country %>% unique()

# running loop over all countries, fitting the model, saving the fit data and making each plot
tryCatch({     
    plot_data <- get_plot_data(country_name = plot_country_name, someDat, CFRBaseline = CFRBaseline)
    prediction <- run_bayesian_model(plot_data)
    
    saveRDS(prediction, tail(.args,1))     
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
)
  

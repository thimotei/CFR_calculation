# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

.args <- if (interactive()) c(
  "inputfile", "...srcs...", "input key",
  "targetfile"
) else commandArgs(trailingOnly = TRUE)

# Load data -----------------------------------------------------
allDat <- read_csv(.args[1])
datakey <- tail(.args, 2)[1]
someDat <- allDat %>% filter(countryterritoryCode == dataKey)

target <- tail(.args, 1)

#source data processing and plotting scripts
srcs <- head(tail(.args, -1), -2)
apply(srcs, 1, source)

# setting baseline level CFR
CFRBaseline <- 1.4
CFREstimateRange <- c(1.2, 1.7)

# Set parameters
mean <- 13
median <- 9.1

mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}

# running loop over all countries, fitting the model, saving the fit data and making each plot
tryCatch({     
    plot_data <- get_plot_data(country_name = "Germany", CFRBaseline = CFRBaseline)
    prediction <- run_bayesian_model(plot_data)
    
    saveRDS(prediction, tail(.args,1))     
    
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
)
  

# setting functions for the delay distribution
muTransform <- function(zMedian){
  mu <- log(zMedian) 
}

sigmaTransform <- function(zMean, mu){
  sigma <- sqrt(2*(log(zMean) - mu))
}

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x, mu, sigma) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}


hospitalisation_to_death_truncated_low <- function(x){
  hospitalisation_to_death_truncated(x, muLow, sigmaLow)
}

hospitalisation_to_death_truncated_mid <- function(x){
  hospitalisation_to_death_truncated(x, muMid, sigmaMid)
}

hospitalisation_to_death_truncated_high <- function(x){
  hospitalisation_to_death_truncated(x, muHigh, sigmaHigh)
}

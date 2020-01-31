
# Read in case and death data 

all_dat <- read.csv("~/Documents/lshtm/nCoV2019/case fatality rate/case_death_data.csv")

# plot naive time-dependent CFR

cfr_naive <- (all_dat$cumulative_deaths)/(all_dat$cumulative_cases)
plot(cfr_naive)
lines(cfr_naive)

# calculate realistic CFR with time-lag built in


meanG <- 10
scaleG <- 1
ft <- function(x)
  {
    dgamma(x, meanG/scaleG, scale = scaleG)
  }

known_all <- 0

for(i in 7:length(all_dat$cases) - 7)
{
  known_i <- 0
  
  for (j in 0:(i - 7))
  {
    known_i <- known_i + (all_dat$cases[i - j]*ft(j))
  }
  
  known_all <- known_all + known_i
}

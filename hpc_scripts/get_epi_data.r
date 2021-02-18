library(data.table)

.debug <- "~/Dropbox/SA2UK"
.args <- if (interactive()) sprintf(c(
  "%s/inputs/epi_data.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)
jhurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
casesurl <- sprintf("%s/time_series_covid19_confirmed_global.csv", jhurl)
deathsurl <- sprintf("%s/time_series_covid19_deaths_global.csv", jhurl)

fetch <- function(url, vn) melt(fread(url)[
  #`Country/Region` %in% c("South Africa", "United Kingdom") &
    `Province/State` == ""
][, -c(1,3,4) ], id.vars = "Country/Region", variable.name = "date", value.name = vn)

# fetch ECDC data; requires network connection
cases.dt <- fetch(casesurl, "cases")
deaths.dt <- fetch(deathsurl, "deaths")

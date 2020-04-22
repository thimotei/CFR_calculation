suppressPackageStartupMessages({
  require(httr)
})

.args <- if (interactive()) c(
  "somedata.csv"
) else commandArgs(trailingOnly = TRUE)

GET(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  authenticate(":", ":", type="ntlm"),
  write_disk(tail(.args,1))
)

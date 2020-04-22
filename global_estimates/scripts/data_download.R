suppressPackageStartupMessages({
  require(tidyverse)
})

.args <- if (interactive()) c(
  "somedata.csv"
) else commandArgs(trailingOnly = TRUE)

res <- tibble()

write_csv(res, file = tail(.args,1))

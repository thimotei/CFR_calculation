here::here() %>% setwd()

EpiNow::copy_report(
  yaml = 'CFR_calculation/global_estimates/man/report-yaml.md',
  report = 'CFR_calculation/global_estimates/Rmd/global_cfr_estimates_public.html',
  date = Sys.Date(),
  lines_to_cut = 1:7,
  report_target = 'cmmid.github.io/topics/covid19/_posts/2020-03-22-global_cfr_estimates.html')



#!bin/bash


# Get or update CMMID

#DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

DIRCMMID='/home/tim/Documents/lshtm/github repos/cmmid.github.io'
DIRCFR="/home/tim/Documents/lshtm/github repos"


cd "$DIRCMMID"

git pull

## Add new report to Repo

cd "$DIRCFR"

Rscript CFR_calculation/global_estimates/scripts/main_script.R

Rscript CFR_calculation/global_estimates/scripts/main_script_temporal.R

R -e "rmarkdown::render('~/Documents/lshtm/github\ repos/CFR_calculation/global_estimates/Rmd/global_cfr_estimates_public.rmd', 'html_document', run_pandoc = FALSE)"

Rscript -e "EpiNow::copy_report(
               yaml = '~/Documents/lshtm/github\ repos/CFR_calculation/global_estimates/man/report-yaml.md',
               report = '~/Documents/lshtm/github\ repos/CFR_calculation/global_estimates/Rmd/global_cfr_estimates_public.html',
               date = Sys.Date(),
               lines_to_cut = 1:7,
               report_target = '~/Documents/lshtm/github\ repos/cmmid.github.io/topics/covid19/severity/_posts/2020-03-22-global_cfr_estimates.html')"

## Update Repo

cd "$DIRCMMID"

git add --all
git commit -m "Update global_cfr_estimates"
git push

cd "$DIRCFR"

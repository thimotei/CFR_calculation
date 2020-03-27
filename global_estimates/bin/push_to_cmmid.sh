#!/bin/bash


# Get or update CMMID
cd ..
base_url="https://github.com/cmmid/"
project="~/Documents/lshtm/github\ repos/cmmid.github.io"

# Update git or clone if not present
if ([ -e $project ]); then
printf "\tUpdating project: %s \n" $project
cd $project
git pull
cd ..
else
  printf "\tCloning project: %s into projects: %s\n" $project $1
git clone "$base_url$project.git"
fi

# Add new report to Repo

cd ~/Documents/lshtm/github\ repos/CFR_calculation/

Rscript global_estimates/scripts/main_script_clean.R

R -e "rmarkdown::render('global_estimates/Rmd/global_cfr_estimates_public.rmd', run_pandoc = FALSE)"

Rscript -e "EpiNow::copy_report(
               yaml = 'global_estimates/man/report-yaml.md',
               report = 'global_estimates/Rmd/global_cfr_estimates_public.html',
               date = Sys.Date(),
               lines_to_cut = 1:7,
               report_target = '../cmmid.github.io/topics/covid19/severity/_posts/2020-03-22-global_cfr_estimates.html')"


# Update Repo

cd ../$project

git add --all
git commit -m "Update global_cfr_estimates"
git push

cd ../$source

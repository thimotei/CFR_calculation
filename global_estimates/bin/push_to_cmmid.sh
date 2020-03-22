#!/bin/bash

# Get or update CMMID
cd ..
base_url="https://github.com/cmmid/"
project="cmmid.github.io"

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

cd CFR_calculation

Rscript -e "EpiNow::copy_report(
               yaml = 'man/report-yaml.md',
               report = 'Rmd/global_cfr_estimates.html',
               date = Sys.Date(),
               lines_to_cut = 1:7,
               report_target = '../cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html')"


# Update Repo

cd ../$project

git add --all
git commit -m "Update global_cfr_estimates"
git push

cd ../$source

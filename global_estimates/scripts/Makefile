-include local.Makefile

R = Rscript $^ $@

something.csv: data_download.R
  ${R}

alltargets.slurm: something.csv
  echo "TODO generate appropriate slurm tasking"

result_%.csv: run_inference.R something.csv
  Rscript $^ $* $@

gather.tar.gz: $(wildcard result_*.csv)
  echo "TODO gather the pieces"

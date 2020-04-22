-include local.Makefile

PATHTOSRC := CFR_calculation/global_estimates/temporal/R

SRCS := $(shell ls ${PATHTOSRC})

SRCFILES := $(patsubst %,${PATHTOSRC}/%.R,${SRCS})

default: testrun

R = Rscript $^ $@

something.csv: data_download.R
  ${R}

alltargets.slurm: something.csv
  echo "TODO generate appropriate slurm tasking"

result_%.rds: run_simulation.R something.csv ${SRCFILES}
  Rscript $^ $* $@

testrun: result_AFG.rds

gather.tar.gz: $(wildcard result_*.csv)
  echo "TODO gather the pieces"

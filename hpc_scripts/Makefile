-include local.Makefile

PATHTOSRC := ../temporal/R

SRCS := $(shell ls ${PATHTOSRC})

SRCFILES := $(patsubst %,${PATHTOSRC}/%,${SRCS})

default: testrun

R = Rscript $^ $@

something.csv: data_download.R
	${R}
	
adjusted_cfr.csv: adjusted_cfr.R age_stratified_cfr.csv
	${R}

alltargets.slurm: something.csv
	echo "TODO generate appropriate slurm tasking"

result_%.rds: run_simulation.R adjusted_cfr.csv something.csv ${SRCFILES}
	Rscript $^ $* $@

testrun: result_AFG.rds

gather.tar.gz: $(wildcard result_*.csv)
	echo "TODO gather the pieces"

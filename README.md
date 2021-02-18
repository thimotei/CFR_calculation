

# Estimating under-ascertainment of symptomatic COVID-19 cases over time

This repository contains code to run the inference framework that estimates
time-varying ascertainment rates of COVID-19 cases ([Russell et
al.](https://doi.org/10.1186/s12916-020-01790-9)). To do so, we use a Gaussian
Process modelling framework, fit to the confirmed COVID-19 death time series
for the country or region in question (see [Russell et
al.](https://doi.org/10.1186/s12916-020-01790-9) for more details on the
methods and limitations involved).  

<!--## Contents of the repository-->

To run the code, first of all clone this repository, using the command

```sh
git clone https://github.com/thimotei/CFR_calculation
```

The time-varying estimates result from fitting a Guassian Process model, which
is implemented in the R libraries <code>greta</code> and <code>greta.gp</code>.
These need to be run from a virtual environment, which is taken care of in the
script the model is run from. Specifically, the user needs to run the following
commands to ensure the necessary packages are installed 
```r
install.packages(c("reticulate", "greta", "greta.gp"))
```
<code>reticulate</code> is required for a virtual environment to
<code>python</code>, as <code>greta</code> requires a virtual environment, as
it uses <code>tensorflow</code> called from this virtual environment.

The user therefore needs to install the correct version of
<code>tensorflow</code> for <code>greta</code>. This is done from R with the
following commands (the same commands are in the main script, but commented out
and need only to be run once):

```r
library(reticulate)
use_condaenv('r-reticulate', required = TRUE)
library(greta)
library(greta.gp)
greta::install_tensorflow(method = "conda",
                          version = "1.14.0",
                          extra_packages = "tensorflow-probability==0.7")
```
Once the user has installed <code>tensorflow</code>, they can run the model
from within the script
```r
scripts/main_script_GP.R
```
which runs the model for a single country or region, specified by the 3-letter
iso-code. The script downloads the latest data from Johns Hopkins COVID-19
dataset [here](https://github.com/CSSEGISandData/COVID-19) and munges the data
into the correct format using this function
```r
R/jhu_data_import.R
```
To run the model at scale, a HPC is used, using the scripts found in 
```r
hpc_scripts
```


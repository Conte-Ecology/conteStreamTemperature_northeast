#!/bin/bash
# bash script to run series of temperature model scripts
# inputs: none
# requires: Rprofile with username and password for sheds database
# requires: ~.pgpass file with host, port, username, and password for database
# config json and validate json

set -eu
set -o pipefail

# create directory for the model run
now=$(date +"%Y-%m-%d")
# rm -r "modelRun_"$now
mkdir -p "modelRun/modelRun_"$now 

dirname="modelRun/modelRun_"$now

# save txt of directory for input into R files
# Rscript create_directory.R 
echo "modelRun/modelRun_"$now > "current_model_run.txt"

# set status log
echo "Script Status" > $dirname"/status_log.txt"

# determine what locations are near impoundments
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting impoundments script (id_impoundment_sites.sh): "$dt >> $dirname"/status_log.txt"
bash id_impoundment_sites.sh sheds $dirname"/"

# determine what locations are potentially tidally influenced
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting tidal script (id_tidal_sites.sh): "$dt >> $dirname"/status_log.txt"
bash id_tidal_sites.sh sheds dan $dirname"/"
 
# Fetch data that are reviewed (exclude tidal & impounded)
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting temperature data retreival (retrieve_db.R): "$dt >> $dirname"/status_log.txt"
Rscript retrieve_db.R $dirname"/temperatureData.RData" $dirname"/covariateData.RData" $dirname"/climateData.RData"

# Fetch daymet data
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting daymet query (daymet_query.sql): "$dt >> $dirname"/status_log.txt"
psql -f $dirname/code/daymet_query.sql -h felek.cns.umass.edu -d sheds -w > $dirname/daymet_results.csv

# Determine breakpoints of synchronized season
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting breakpoints (breakpoints.R): "$dt >> $dirname"/status_log.txt"
Rscript breakpoints.R $dirname"/temperatureData.RData" $dirname"/daymet_results.csv" $dirname"/springFallBPs.RData"

# prepare data for use in model (filter and standardize)
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting model data prep (prepare_model_data.R): "$dt >> $dirname"/status_log.txt"
Rscript prepare_model_data.R $dirname"/temperatureData.RData" $dirname"/daymet_results.csv" $dirname"/covariateData.RData" $dirname"/springFallBPs.RData" $dirname"/tempDataSync.RData"

# run the model
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting model (run_model.R): "$dt >> $dirname"/status_log.txt"
Rscript run_model.R $dirname"/tempDataSync.RData" $dirname"/jags.RData" $dirname"/covariate_list.RData"

# mcmc and model diagnostics
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting diagnostic plots (mcmc_diagnostics.R): "$dt >> $dirname"/status_log.txt"
Rscript mcmc_diagnostics.R $dirname"/jags.RData"

# summarize (iterations)
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting model summary (summarize_iterations.R): "$dt >> $dirname"/status_log.txt"
Rscript summarize_iterations.R $dirname"/tempDataSync.RData" $dirname"/jags.RData" $dirname"/covariate_list.RData" $dirname"/coef.RData"

# validate model
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting validation (validate_model.R): "$dt >> $dirname"/status_log.txt"
Rscript validate_model.R $dirname"/tempDataSync.RData" $dirname"/covariate_list.RData" $dirname"/coef.RData" $dirname"/rmse_table.RData"

# calculate derived metrics for all catchments
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting predictions (predict_temperature_parallel.R): "$dt >> $dirname"/status_log.txt"
Rscript predict_temperatures_parallel.R $dirname"/coef.RData" $dirname"/tempDataSync.RData" $dirname"/covariate_list.RData" $dirname"/springFallBPs.RData"



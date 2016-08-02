#!/bin/bash
# bash script to run series of temperature model scripts
# inputs: none
# requires: Rprofile with username and password for sheds database
# requires: ~.pgpass file with host, port, username, and password for database
# config json and validate json

set -eu
set -o pipefail

dirname="modelRun/modelRun_2016-07-15"

# calculate derived metrics for all catchments
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "re-starting predictions (predict_temperature_parallel.R): "$dt >> $dirname"/status_log.txt"
Rscript code/predict_temperatures_parallel.R $dirname"/coef.RData" $dirname"/tempDataSync.RData" $dirname"/covariate_list.RData" $dirname"/springFallBPs.RData"


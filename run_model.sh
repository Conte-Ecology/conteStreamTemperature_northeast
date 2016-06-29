#!/bin/bash
# bash script to run series of temperature model scripts
# inputs: none
# requires: Rprofile with username and password for sheds database

set -eu
set -o pipefail

# create directory for the model run
now=$(date +"%Y-%m-%d")
# rm -r "modelRun_"$now
mkdir -p "modelRun_"$now 

dirname="modelRun_"$now

# save txt of directory for input into R files
# Rscript create_directory.R 
echo "modelRun_"$now > "current_model_run.txt"

# # get username
# echo "Enter your username for the sheds database, followed by [ENTER]:"
# read USER
# 
# # get password
# echo "Enter your password for the sheds database, followed by [ENTER]:"
# read PASSWORD

# set status log
echo "Script Status" > $dirname"/status_log.txt"

# determine what locations are near impoundments
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting impoundments script: "$dt >> $dirname"/status_log.txt"
bash id_impoundment_sites.sh sheds modelRun_$now/ # test_impoundment_locations.csv

# dt=$(date '+%Y-%m-%d %H:%M:%S');
# echo "starting impounded sql script: "$dt >> $dirname"/status_log.txt"
# psql -h felek.cns.umass.edu -U dan -d sheds -f locations_impounded.sql -q -w > $dirname"impoundment_sites.csv"

# determine what locations are potentially tidally influenced
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting tidal script: "$dt >> $dirname"/status_log.txt"
bash id_tidal_sites.sh sheds dan modelRun_$now/
 
# get impoundment and tidal locations
# dt=$(date '+%Y-%m-%d %H:%M:%S');
# echo "starting tidal and impounded script: "$dt >> $dirname"/status_log.txt"
# psql -h felek.cns.umass.edu -U dan -d sheds -f locations_impounded_tidal.sql -q -w > $dirname"/exclude_locations.csv"

# Fetch data that are reviewed (exclude tidal & impounded)
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting temperature data retreival (retrieve_db.R): "$dt >> $dirname"/status_log.txt"
Rscript retrieve_db.R ./modelRun_$now/temperatureData.RData ./modelRun_$now/covariateData.RData ./modelRun_$now/climateData.RData

# Fetch daymet data
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting daymet query daymet_query.sql: "$dt >> $dirname"/status_log.txt"
psql -f $dirname/code/daymet_query.sql -d sheds -w > $dirname/daymet_results.csv

# Determine breakpoints of synchronized season
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting breakpoints.R: "$dt >> $dirname"/status_log.txt"
Rscript breakpoints.R $dirname"/temperatureData.RData" $dirname"/daymet_results.csv" $dirname"/springFallBPs.RData"

# prepare data for use in model (filter and standardize)
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting prepare_model_data.R: "$dt >> $dirname"/status_log.txt"
Rscript prepare_model_data.R $dirname"/temperatureData.RData" $dirname"/daymet_results.csv" $dirname"/springFallBPs.RData" $dirname"/tempDataSync.RData"

# run the model

# validate model

# calculate derived metrics for all catchments
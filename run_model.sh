#!/bin/bash
# bash script to run series of temperature model scripts
# inputs: none
# requires: Rprofile with username and password for sheds database

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
echo "Script Status\n" > $dirname"/status_log.txt"

# determine what locations are near impoundments - datababse password
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting impoundments script: "$dt >> $dirname"/status_log.txt"

bash id_impoundment_sites.sh sheds modelRun_$now/ # test_impoundment_locations.csv
 
# determine what locations are potentially tidally influenced
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting tidal script: "$dt >> $dirname"/status_log.txt"
bash id_tidal_sites.sh sheds dan modelRun_$now/
 
# Fetch non-tidal, non-impoundment data that are reviewed
dt=$(date '+%Y-%m-%d %H:%M:%S');
echo "starting temperature data retreival (retrieve_db.R): "$dt >> $dirname"/status_log.txt"
# Rscript retrieve_db.R ./modelRun_$now/temperatureData.RData ./modelRun_$now/covariateData.RData ./modelRun_$now/climateData.RData

# Determine breakpoints of synchronized season

# prepare data for use in model (filter and standardize)

# run the model

# validate model

# calculate derived metrics for all catchments
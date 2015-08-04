# Retrieve data from postgres database
#
# requires working directory
#
# returns three RData files: observed temperature time series, landscape/landuse, and climate data from daymet
#
# usage: $ Rscript derive_metrics.R <input ??? json> <output temperatureData rdata> <output covariateData rdata> <output climateData rdata>
# example: $ Rscript retrieve_db.R ./wd??? ./temperatureData.RData ./covariateData.RData ./climateData.RData

library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(RPostgreSQL)
library(ggplot2)
# install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

data_dir <- getOption("SHEDS_DATA")
  
args <- commandArgs(trailingOnly = TRUE)

wd <- args[1]
#if (!file.exists(csv_file)) {
#  stop(paste0('Could not find temperatureData csv file: ', csv_file))
#}
output_file1 <- args[2]
if (file.exists(output_file1)) {
  warning(paste0('Output file 1 already exists, overwriting: ', output_file1))
}
output_file2 <- args[3]
if (file.exists(output_file2)) {
  warning(paste0('Output file 2 already exists, overwriting: ', output_file2))
}
output_file3 <- args[4]
if (file.exists(output_file3)) {
  warning(paste0('Output file 3 already exists, overwriting: ', output_file3))
}

if(exists(wd)) setwd(wd)

if(length(args) < 1) {
  print("No arguments imported. Using default input and output files and directories")
  output_file1 <- file.path("localData/temperatureData.RData")
  output_file2 <- file.path("localData/covariateData.RData")
  output_file3 <- file.path("localData/climateData.RData")
}

#------------------set up database connections--------------------
# connect to database source
  db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# table references
tbl_locations <- tbl(db, 'locations') %>%
  rename(location_id=id, location_name=name, location_description=description) %>%
  select(-created_at, -updated_at)

tbl_agencies <- tbl(db, 'agencies') %>%
  rename(agency_id=id, agency_name=name) %>%
  select(-created_at, -updated_at)

tbl_series <- tbl(db, 'series') %>%
  rename(series_id=id) %>%
  select(-created_at, -updated_at)

tbl_variables <- tbl(db, 'variables') %>%
  rename(variable_id=id, variable_name=name, variable_description=description) %>%
  select(-created_at, -updated_at)

tbl_values <- tbl(db, 'values') %>%
  rename(value_id=id)

#tbl_daymet <- tbl(db, 'daymet')

tbl_covariates <- tbl(db, 'covariates')

# list of agencies to keep
# keep_agencies <- c('MADEP', 'MAUSGS')

##### Need way to filter data that has a "yes" QAQC flag

# fetch locations
df_locations <- left_join(tbl_locations, tbl_agencies, by=c('agency_id'='agency_id')) %>%
  # filter(agency_name %in% keep_agencies) %>%
  filter(agency_name != "TEST") %>%
  rename(featureid=catchment_id) %>%
  collect
summary(df_locations)
unique(df_locations$agency_name)

#------------------------fetch temperature data-------------------

start.time <- Sys.time()

df_values <- tbl_values %>%
  left_join(tbl_series, by = c("series_id")) %>%
  left_join(dplyr::select(tbl_variables, variable_id, variable_name),
            by=c('variable_id'='variable_id')) %>%
  dplyr::select(-file_id) %>%
  filter(location_id %in% df_locations$location_id,
         variable_name=="TEMP") %>%
  collect %>%
  mutate(datetime=with_tz(datetime, tzone='EST'),
         date = as.Date(datetime),
         series_id = as.character(series_id)) %>%
  rename(temp = value)

Sys.time() - start.time # only 3.5 minutes for 16 million records from New England

summary(df_values)

if(!file.exists(file.path(getwd(), "localData"))) dir.create(file.path(getwd(), "localData"))
saveRDS(df_values, file = file.path(getwd(), "localData/df_values.RData"))

# df_values <- readRDS(file = file.path(getwd(), "localData/df_values.RData"))

#--------------------------QAQC---------------------------

#df_values <- obs_freq(df_values)

# Flag data with problems
df_values <- df_values %>%
  obs_freq(.) %>%
  flag_incomplete(.) %>%
  flag_hourly_rise(.) %>%
  flag_cold_obs(.) %>%
  flag_hot_obs(.) %>%
  flag_extreme_obs(.) %>%
  flag_interval(.) %>%
  convert_neg(.) %>%
  dplyr::select(-row, -temp_prev, -series_prev, -series_start)

# output flags table

# Filter based on subdaily flags
df_values2 <- df_values %>%
  group_by(series_id, date) %>%
  filter(flag_incomplete == "FALSE", # make all != TRUE to include NA?
         flag_cold_obs == "FALSE",
         flag_hot_obs == "FALSE",
         flag_interval == "FALSE" | is.na(flag_interval),
         abs(d_temp) < 10 | is.na(d_temp))


# Convert to daily

df_values3 <- df_values2 %>%
  group_by(series_id, date, location_id, agency_id) %>%
  filter(flagged == "FALSE") %>%
  summarise(meanTemp = mean(temp), 
            maxTemp = max(temp), 
            minTemp = min(temp), 
            #obs_per_day = mean(obs_per_day),  # change mean to median when dplyr fixed
            n_obs = n()) %>%
  rename(temp = meanTemp)
summary(df_values3)
dim(df_values3)

# QAQC on daily 

df_values3 <- df_values3 %>%
  flag_daily_rise(.) %>%
  flag_cold_days(.) %>%
  flag_hot_days(.) %>%
  flag_extreme_days(.) %>%
  dplyr::filter(flag_cold_days == "FALSE",
                abs(d_temp) < 15 | is.na(d_temp))

# output daily flags

# End QAQC

# Get location and agency data and join to temperature data
# df_locations <- collect(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id))

df_agencies <- collect(tbl_agencies)

temperatureData <- df_values3 %>%
  left_join(df_locations, by = c('location_id', 'agency_id')) %>%
  #left_join(df_agencies, by = 'agency_id') %>%
  select(location_id, agency_name, location_name, latitude, longitude, featureid, date, temp, maxTemp, minTemp, n_obs) %>%
  mutate(agency_name=factor(agency_name),
         location_name=factor(location_name),
         year = year(date))

# create temperatureData input dataset
summary(temperatureData)

#Summarise to featureid
# for all current analyses just need mean within reach (featureid)

# does mean make sense because could result in discontinuities in time series within a reach if one logger was in a seep and other not

# Need to filter by agency and such by here because can't do it later if multiple agencies in the same reach (featureid)
temperatureData2 <- temperatureData %>%
  dplyr::group_by(featureid, date, year) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T), 
                   tempMax = mean(maxTemp, na.rm = T),
                   tempMin = mean(minTemp, na.rm = T),
                   n_obs = n())

years <- unique(temperatureData2$year)
featureids <- unique(temperatureData2$featureid)
featureids <- featureids[!is.na(featureids)]

#-----------------------fetch covariates-----------------------
# check upstream vs local covariates

# fetch covariates
start.time <- Sys.time()

df_covariates <- filter(tbl_covariates, featureid %in% featureids) %>%
  collect %>%
  spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

Sys.time() - start.time

# create covariateData input dataset
covariateData <- left_join(select(df_locations, location_id, location_name, latitude, longitude, featureid),
                           df_covariates,
                           by=c('featureid'='featureid')) %>%
  mutate(location_name=factor(location_name))
summary(covariateData)

# need to organize covariates into upstream or local by featureid
upstream <- covariateData %>%
  group_by(featureid) %>%
  filter(zone == "upstream") %>%
  select(-zone, -location_id, -location_name) %>%
  summarise_each(funs(mean))



#---------------------daymet climate data-------------------------

# too big/slow to pull through R so make the query and export that. The resulting sql script can then be run via command line or within a bash script or make file

featureids_string <- paste(featureids, collapse=', ')
years_string <- paste(years, collapse=', ')

qry <- paste0("COPY(SELECT featureid, date_part('year', date) as year, date, tmax, tmin, prcp, dayl, srad, swe FROM daymet WHERE featureid IN (", featureids_string, ") AND date_part('year', date) IN (",years_string, ") ) TO STDOUT CSV HEADER;")  # "select * from whatever where featureid in (80001, 80002, 80003)"

if(!file.exists(file.path(getwd(), "code"))) dir.create(file.path(getwd(), "code"))
cat(qry, file = "code/daymet_query.sql")


#----------------save files---------------------

saveRDS(temperatureData2, file=output_file1)
saveRDS(upstream, file=output_file2)
#saveRDS(climateData, file=output_file3)


#---------------cleaning---------------------
# remove variables and gc()
rm(list = c("df_values", "df_values2", "df_values3", "temperatureData"))
gc()


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

data_dir <- getOption("SHEDS_DATA")

args <- NA_character_

if(file.exists("/conte")) {
  
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

# connect to database source
db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
} else {
  
  db <- src_postgres(dbname='conte_dev', host='128.119.112.36', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
}

if(!exists(args)) {
  output_file1 <- file.path("localData/temperatureData.RData")
  output_file2 <- file.path("localData/covariateData.RData")
  output_file3 <- file.path("localData/climateData.RData")
}

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
tbl_daymet <- tbl(db, 'daymet')
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

# fetch temperature data
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
summary(df_values)

if(!file.exists(file.path(getwd(), "localData"))) dir.create(file.path(getwd(), "localData"))
saveRDS(df_values, file = file.path(getwd(), "localData/df_values.RData"))

# df_values <- readRDS(file = file.path(getwd(), "localData/df_values.RData"))

#------------QAQC---------------------------

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

# output flags table----------

# Filter based on subdaily flags
df_values <- df_values %>%
  group_by(series_id, date) %>%
  filter(flag_incomplete == "FALSE",
         flag_cold_obs == "FALSE",
         flag_hot_obs == "FALSE",
         flag_interval == "FALSE",
         abs(d_temp) < 10)


# Convert to daily

df_values2 <- df_values %>%
  group_by(series_id, date, location_id, agency_id) %>%
  filter(flagged == "FALSE") %>%
  summarise(meanTemp = mean(temp), 
            maxTemp = max(temp), 
            minTemp = min(temp), 
            #obs_per_day = mean(obs_per_day),  # change mean to median when dplyr fixed
            n_obs = n()) %>%
  rename(temp = meanTemp)
summary(df_values2)

# QAQC on daily 

df_values2 <- df_values2 %>%
  flag_daily_rise(.) %>%
  flag_cold_days(.) %>%
  flag_hot_days(.) %>%
  flag_extreme_days(.)

# output daily flags

# End QAQC

# Get location and agency data and join to temperature data
# df_locations <- collect(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id))

df_agencies <- collect(tbl_agencies)

temperatureData <- df_values2 %>%
  left_join(df_locations, by = c('location_id', 'agency_id')) %>%
  #left_join(df_agencies, by = 'agency_id') %>%
  select(location_id, agency_name, location_name, latitude, longitude, featureid, date, temp, maxTemp, minTemp, n_obs) %>%
  mutate(agency_name=factor(agency_name),
         location_name=factor(location_name),
         year = year(date))

# create temperatureData input dataset
summary(temperatureData)


######## check upstream vs local covariates

# fetch covariates
df_covariates <- filter(tbl_covariates, featureid %in% df_locations$featureid) %>%
  collect %>%
  spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# create covariateData input dataset
covariateData <- left_join(select(df_locations, location_id, location_name, latitude, longitude, featureid),
                           df_covariates,
                           by=c('featureid'='featureid')) %>%
  mutate(location_name=factor(location_name))
summary(covariateData)

# create climateData input dataset - need full year
dates <- unique(temperatureData$date)
years <- unique(year(temperatureData$date))

climate <- tbl_daymet %>%
  #filter(featureid %in% df_locations$featureid & date %in% dates)
  filter(featureid %in% df_locations$featureid) %>%
  mutate(year = DATE_PART("year", date)) 

climate_filter <- climate %>%
  filter(featureid %in% df_locations$featureid & year %in% years) # can't chain this with above. Should probably just write direct SQL code.

drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname="conte_dev", host="127.0.0.1", user="conte", password="conte")
con <- dbConnect(drv, dbname="conte_dev", host="felek.cns.umass.edu", user="conte", password="conte")
qry <- "SELECT DISTINCT featureid FROM daymet;"
qry <- "
WITH daymet_year AS (
  SELECT date_part('year', datetime) as year,
         count(value) as n, series_id
  FROM daymet
  GROUP BY featureid, year
), loc_year AS (
  SELECT l.catchment_id AS featureid, var.name as variable, 
         vy.year as year, SUM(vy.n) as n_values, COUNT(l.*) as n_locations
  WHERE var.name='TEMP'
)"
result <- dbSendQuery(con, qry)
catchments <- fetch(result, n=-1)

# larger than needed because collectes for all years and featureid not year-featureid combos
climateData <- collect(climate) %>%
  filter(featureid %in% df_locations$featureid & year %in% years) # can't chain this with above. Should probably just write direct SQL code.


#---------Summarise to featureid------
# for all current analyses just need mean within reach (featureid)

# does mean make sense because could result in discontinuities in time series within a reach if one logger was in a seep and other not

# Need to filter by agency and such by here because can't do it later if multiple agencies in the same reach (featureid)
temperatureData2 <- temperatureData %>%
  dplyr::group_by(featureid, date) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T), 
                   maxTemp = mean(maxTemp, na.rm = T),
                   minTemp = mean(minTemp, na.rm = T),
                   n_obs = n())


####### Do we want to put these in a subfolder?

saveRDS(temperatureData2, file=output_file1)
saveRDS(covariateData, file=output_file2)
saveRDS(climateData, file=output_file3)


# remove variables and gc()




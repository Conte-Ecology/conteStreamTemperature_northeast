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
  rename(temp = values)
summary(df_values)

saveRDS(df_values, file = "df_values.RData")

#------------QAQC---------------------------

#df_values <- obs_freq(df_values)

# Flag data with problems
df_values <- df_values %>%
  obs_freq(.)
flag_incomplete(.) %>%
  flag_hourly_rise(.) %>%
  flag_cold_obs(.) %>%
  flag_hot_obs(.) %>%
  flag_extreme_obs(.) %>%
  flag_interval(.) %>%
  convert_neg(.)

# output flags table----------

# Convert to daily

df_values <- df_values %>%
  group_by(series_id, date) %>%
  filter(flagged == "FALSE") %>%
  summarise(temp = mean(value), maxTemp = max(value), minTemp = min(value), obs_per_day = median(obs_per_day), n_obs = n())
summary(df_values)

# QAQC on daily 

# output daily flags



#----------old can cut if rest works---------
samples_series_day <- df_values %>%
  dplyr::group_by(series_id, date) %>%
  dplyr::summarise(obs_per_day = n())
summary(samples_series_day)

median_samples <- samples_series_day %>%
  dplyr::group_by(series_id) %>%
  dplyr::summarise(median_freq = median(obs_per_day), min_n90 = median_freq*0.9)
summary(median_samples)

series_90 <- samples_series_day %>%
  dplyr::left_join(median_samples, by = c("series_id")) %>%
  dplyr::filter(obs_per_day > min_n90)
summary(series_90)

foo <- filter(df_values, filter = series_id == 900)
ggplot(foo, aes(datetime, value)) + geom_point()

df_values <- df_values %>%
  left_join(series_90, by = c("series_id", "date")) %>%
  filter(obs_per_day > min_n90) 

df_values <- df_values %>%
  group_by(series_id, date, location_id, agency_id) %>%
  filter(flagged == "FALSE") %>%
  filter(variable_name == "TEMP") %>%
  summarise(temp = mean(value), maxTemp = max(value), minTemp = min(value), obs_per_day = median(obs_per_day))
summary(df_values)
#------------end of old cut-----------------

df_locations <- collect(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id))

df_agencies <- collect(tbl_agencies)

temperatureData <- df_values %>%
  left_join(df_locations, by = 'location_id') %>%
  left_join(df_agencies, by = 'agency_id') %>%
  select(location_id, agency_name, location_name, latitude, longitude, featureid, date, temp, maxTemp, minTemp, obs_per_day, flagged) %>%
  mutate(agency_name=factor(agency_name),
         location_name=factor(location_name))

# If obs_per_day = 1, we assume that this is a mean temperature. Therefore the min and max for those days should be NA
# Can't do ifelse with an NA replace in dplyr because it changes the data types
temperatureData <- temperatureData %>%
  mutate(maxTemp = ifelse(minTemp == maxTemp, -9999, maxTemp)) %>%
  mutate(minTemp = ifelse(maxTemp == -9999, -9999, minTemp))

# solution from Hadley - use NA_real_
temperatureData <- temperatureData %>%
  mutate(maxTemp = ifelse(maxTemp > -10 | is.na(maxTemp), maxTemp, NA_real_), 
         minTemp = ifelse(minTemp > -10, minTemp, NA_real_),
         temp = ifelse(temp > -10, temp, NA_real_)
  )

# Need to deal with water temperature between -10 - 0 to decide out of water = NA vs. imperfect or in ice and should = 0

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

# create climateData input dataset
dates <- unique(temperatureData$date)

climate <- tbl_daymet %>%
  filter(featureid %in% df_locations$featureid & date %in% dates)

climateData <- collect(climate)

####### Do we want to put these in a subfolder?

saveRDS(temperatureData, file=output_file1)
saveRDS(covariateData, file=output_file2)
saveRDS(climateData, file=output_file3)

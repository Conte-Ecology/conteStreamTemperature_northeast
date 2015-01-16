# Creates masterData, covariateData input binary files
#
# usage: $ Rscript retrieve_db.R <output temperatureData rdata> <output covariateData rdata> <output xlimateData rdata>
# example: $ Rscript retreive_db.R ./temperatureData.RData ./covariateData.RData ./climateData.RData

library(dplyr)
library(tidyr)
library(lubridate)
library(RPostgreSQL)
library(ggplot2)
library(jsonlite)

# replace keep_agencies below with something imported via json
# keep_agencies <- fromJSON('model_config.json') # make config file for list of agencies to include in analysis (or states, etc.)

# parse command line arguments
output_file1 <- args[1]
if (file.exists(output_file1)) {
  warning(paste0('Output file already exists, overwriting: ', output_file1))
}

output_file2 <- args[2]
if (file.exists(output_file2)) {
  warning(paste0('Output file already exists, overwriting: ', output_file2))
}

output_file3 <- args[3]
if (file.exists(output_file3)) {
  warning(paste0('Output file already exists, overwriting: ', output_file3))
}

# connect to database source
db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user='conte', password='conte')

# db <- src_postgres(dbname='conte_dev', host='felek.cns.umass.edu', port='5432', user='conte', password='conte')

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
df_agencies <- collect(tbl_agencies)
#keep_agencies <- c('MADEP', 'MAUSGS', 'MEDMR')
keep_agencies <- as.character(filter(df_agencies, agency_name != "TEST")$agency_name)

# fetch locations
df_locations <- left_join(tbl_locations, tbl_agencies, by=c('agency_id'='agency_id')) %>%
  filter(agency_name %in% keep_agencies) %>%
  rename(featureid=catchment_id) %>%
  collect
summary(df_locations)
unique(df_locations$agency_name)

# fetch covariates
df_covariates <- filter(tbl_covariates, featureid %in% df_locations$featureid) %>%
  collect %>%
  spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# fetch temperature data
tbl_values <- left_join(tbl_series,
                        select(tbl_variables, variable_id, variable_name),
                        by=c('variable_id'='variable_id')) %>%
  select(-file_id) %>%
  filter(location_id %in% df_locations$location_id,
         variable_name=="TEMP") %>%
  left_join(tbl_values,
            by=c('series_id'='series_id')) %>%
  left_join(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id),
            by=c('location_id'='location_id')) %>%
  left_join(tbl_agencies,
            by=c('agency_id'='agency_id')) %>%
  mutate(year = date_part('year', datetime))

df_values <- collect(tbl_values) 
df_values <- df_values %>%
  mutate(datetime=with_tz(datetime, tzone='EST'))
summary(df_values)

# create temperatureData input dataset
temperatureData <- select(df_values, location_id, agency_name, location_name, latitude, longitude, featureid, variable_name, datetime, value, flagged) %>%
  mutate(agency_name=factor(agency_name),
         location_name=factor(location_name),
         variable_name=factor(variable_name))
summary(temperatureData)

# create covariateData input dataset
covariateData <- left_join(select(df_locations, location_id, location_name, latitude, longitude, featureid),
                           df_covariates,
                           by=c('featureid'='featureid')) %>%
  mutate(location_name=factor(location_name))
summary(covariateData)

# create climateData input dataset (too big without prefilter or smaller join)

# SQL Solution
drv <- dbDriver("PostgreSQL")

# create connection
con <- dbConnect(drv, dbname="conte_dev", host="127.0.0.1", user="conte", password="conte")

# con <- dbConnect(drv, dbname="conte_dev", host="felek.cns.umass.edu", user="conte", password="conte")

# create sql query string
qry <- "WITH values_year AS (
  SELECT date_part('year', datetime) as year,
         count(value) as n, series_id
  FROM values
  GROUP BY series_id, year
), loc_year AS (
  SELECT l.catchment_id AS featureid, var.name as variable, 
         vy.year as year, SUM(vy.n) as n_values, COUNT(l.*) as n_locations
  FROM series s
  LEFT JOIN values_year vy ON s.id=vy.series_id
  LEFT JOIN locations l ON s.location_id=l.id
  LEFT JOIN variables var ON s.variable_id=var.id
  WHERE var.name='TEMP'
  GROUP BY l.catchment_id, var.name, vy.year
)

SELECT ly.year, ly.n_values, ly.n_locations, 
       ly.featureid, d.featureid, d.date, d.tmax, d.tmin, d.prcp, d.dayl, d.srad, d.vp, d.swe
FROM daymet d
INNER JOIN loc_year ly
ON d.featureid=ly.featureid
  AND date_part('year', d.date)=ly.year
ORDER BY d.featureid, d.date;"

# submit query
result <- dbSendQuery(con, qry)

# fetch results (n=-1 means return all rows, use n=5 to return just first 5 rows, for example)
climateData <- fetch(result, n=-1)

# Save files for use in subsequent scripts
saveRDS(temperatureData, file='temperatureData.RData')
saveRDS(covariateData, file='covariateData.RData')
saveRDS(climateData, file='climateData.RData')





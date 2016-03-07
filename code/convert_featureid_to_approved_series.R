library(pander)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(lubridate)
library(conteStreamTemperature)
library(texreg)
library(stargazer)
#library(tables)

local_dir <- "/localData_2016-01-19"
data_dir <- paste0(getwd(), local_dir)

load(file.path(data_dir, "tempDataSync.RData"))


db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# table references
tbl_locations <- tbl(db, 'locations') %>%
  rename(location_id=id, location_name=name, location_description=description) %>%
  select(-created_at, -updated_at)

tbl_agencies <- tbl(db, 'agencies') %>%
  rename(agency_id=id, agency_name=name) %>%
  select(-created_at, -updated_at)
df_agencies <- collect(tbl_agencies)

tbl_series <- tbl(db, 'series') %>%
  rename(series_id=id) %>%
  select(-created_at, -updated_at)
df_series <- dplyr::collect(tbl_series)

tbl_variables <- tbl(db, 'variables') %>%
  rename(variable_id=id, variable_name=name, variable_description=description) %>%
  select(-created_at, -updated_at)

tbl_values <- tbl(db, 'values') %>%
  rename(value_id=id)

#tbl_daymet <- tbl(db, 'daymet')

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


# find series id used in the model and in the validation
fit_series
fit_featureid <- data.frame(featureid = unique(tempDataSyncS$featureid), use = "fit", stringsAsFactors = FALSE)
valid_series
valid_featureid <- data.frame(featureid = unique(tempDataSyncValidS$featureid), use = "valid", stringsAsFactors = FALSE)

ids <- dplyr::bind_rows(fit_featureid, valid_featureid)

str(df_series)
str(df_locations)



df_approved <- df_series %>%
  left_join(df_locations) %>%
  dplyr::filter(featureid %in% unique(ids$featureid)) %>%
  dplyr::left_join(ids)

str(df_approved)
length(unique(df_approved$featureid))
length(unique(ids$featureid)) 

write.csv(df_approved, file = paste0(data_dir, "/approved_series.csv"), row.names = FALSE)


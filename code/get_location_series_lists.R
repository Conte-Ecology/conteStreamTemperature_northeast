data_dir <- "localData_2016-02-26_newDelineation/"

load(file.path(data_dir, "tempDataSync.RData"))
series_used <- read.csv(paste0(data_dir, "series_used.csv"), header = TRUE, stringsAsFactors = FALSE)

str(series_used)


# connect to database source
db <- src_postgres(dbname='sheds_new', host='osensei.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# table references
tbl_locations <- tbl(db, 'locations') %>%
  dplyr::rename(location_id=id, location_name=name, location_description=description) %>%
  dplyr::select(-created_at, -updated_at)

df_locations <- dplyr::collect(tbl_locations)

dbDisconnect(db$con)

db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_agencies <- tbl(db, 'agencies') %>%
  dplyr::rename(agency_id=id, agency_name=name) %>%
  dplyr::select(-created_at, -updated_at)
df_agencies <- collect(tbl_agencies)

series <- unique(series_used$series_id)
tbl_series <- tbl(db, 'series') %>%
  dplyr::select(-created_at, -updated_at) %>%
  dplyr::filter(id %in% series)

df_series <- dplyr::collect(tbl_series) %>%
  dplyr::rename(series_id=id)

dbDisconnect(db$con)

# fetch locations
df_locations <- left_join(df_locations, df_agencies, by=c('agency_id'='agency_id')) %>%
  # filter(agency_name %in% keep_agencies) %>%
  dplyr::filter(agency_name != "TEST") %>%
  dplyr::rename(featureid=catchment_id)
str(df_locations)
summary(df_locations)
unique(df_locations$agency_name)


# Get locations from series used

str(df_series)
str(df_locations)

# combined list of featureid for fitting and validation
df_fitted_id <- tempDataSyncS %>%
  dplyr::select(featureid) %>%
  dplyr::mutate(fitted = 1) %>%
  dplyr::distinct()

df_valid_id <- tempDataSyncValidS %>%
  dplyr::select(featureid) %>%
  dplyr::mutate(validation = 1) %>%
  dplyr::distinct()

# problem that sites can be used for fitting and validation in different years

df_featureid <- dplyr::bind_rows(df_fitted_id, df_valid_id)
featureids <- unique(df_featureid$featureid)

df <- df_series %>%
  dplyr::left_join(df_locations) %>%
  dplyr::filter(featureid %in% featureids)

# could get amount of data used by each agency
df_agency_count <- df %>%
  dplyr::select(agency_name, description, public, value_count) %>%
  dplyr::group_by(agency_name, description, public) %>%
  dplyr::summarise(value_count_sum = sum(value_count))

# df_agency_count <- dplyr::mutate(df_agency_count, )

write.csv(df_agency_count, file = paste0(data_dir, "agency_data_used.csv"), row.names = FALSE)

# get locations with fitted, validation, both columns
df_fit_valid <- df %>%
  dplyr::select(location_id, agency_name, latitude, longitude, featureid) %>%
  dplyr::left_join(df_fitted_id) %>%
  dplyr::left_join(df_valid_id) %>%
  dplyr::mutate(fitted = ifelse(is.na(fitted), 0, fitted),
                validation = ifelse(is.na(validation), 0, validation))
  
df_fit_valid <- df_fit_valid %>%
  dplyr::mutate(use = ifelse(fitted == 1 & validation == 1, "both",
                             ifelse(fitted == 1 & validation == 0, "fitted",
                                    ifelse(fitted == 0 & validation == 1, "validation", NA))),
                featureid = as.integer(featureid))
summary(df_fit_valid)
df_fit_valid %>%
  dplyr::group_by(use) %>%
  dplyr::summarise(count = n())
length(unique(df_fit_valid$location_id))
length(unique(df_fit_valid$featureid))

# export fitted and validation locations with lat lon for mapping
write.csv(df_fit_valid, file = paste0(data_dir, "locations_used.csv"), row.names = FALSE)









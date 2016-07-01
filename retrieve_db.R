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
library(devtools)
install_github("Conte-Ecology/conteStreamTemperature", quiet = TRUE)
library(conteStreamTemperature)

#data_dir <- getOption("SHEDS_DATA")
# data_dir <- paste0("localData_2016-05-29_newDelineation")
# 
# if(!file.exists(file.path(getwd(), data_dir))) dir.create(file.path(getwd(), data_dir))
#   

library(jsonlite)
config <- fromJSON('model_config.json')

# get current model run directory
data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1])

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# until running as a bash script add the files here
if(length(args) < 1) {
  print("No arguments imported. Using default input and output files and directories")
  args <- c(paste0(data_dir, "/temperatureData.RData"), paste0(data_dir, "/covariateData.RData"), paste0(data_dir, "/climateData.RData"))
}

# input_file1 <- args[1]
# data_dir <- as.character(read.table(paste0(input_file1, "/", input_file1, ".txt"), stringsAsFactors = FALSE)[1,1])

output_file1 <- args[1]
if (file.exists(output_file1)) {
  warning(paste0('Output file 1 already exists, overwriting: ', output_file1))
}
output_file2 <- args[2]
if (file.exists(output_file2)) {
  warning(paste0('Output file 2 already exists, overwriting: ', output_file2))
}
output_file3 <- args[3]
if (file.exists(output_file3)) {
  warning(paste0('Output file 3 already exists, overwriting: ', output_file3))
}
# output_file4 <- args[4]
# if (file.exists(output_file4)) {
#   warning(paste0('Output file 4 already exists, overwriting: ', output_file4))
# }
# 
# # save directory so don't have to change in every subsequent file
# save(data_dir, file = paste0(output_file4, "/data_dir.RData"))

#------------------set up database connections--------------------
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
# table references
tbl_locations <- tbl(db, 'locations') %>%
  dplyr::rename(location_id=id, location_name=name, location_description=description) %>%
  dplyr::select(-created_at, -updated_at)

df_locations <- dplyr::collect(tbl_locations)

tbl_agencies <- tbl(db, 'agencies') %>%
  dplyr::rename(agency_id=id, agency_name=name) %>%
  dplyr::select(-created_at, -updated_at)
df_agencies <- collect(tbl_agencies)

tbl_series <- tbl(db, 'series') %>%
  dplyr::rename(series_id=id) %>%
  dplyr::select(-created_at, -updated_at, -flags)

# tbl_values <- tbl(db, 'values') %>%
#   dplyr::rename(value_id=id)

tbl_variables <- tbl(db, 'variables') %>%
  dplyr::rename(variable_id=id, variable_name=name, variable_description=description) %>%
  dplyr::select(-created_at, -updated_at)

df_variables <- dplyr::collect(tbl_variables)

#tbl_daymet <- tbl(db, 'daymet')

# list of agencies to keep
# keep_agencies <- c('MADEP', 'MAUSGS')

##### Need way to filter data that has a "yes" QAQC flag

#--------------------- Filter to Cleaned locations/series --------------

# df_clean_series <- read.csv(paste0(data_dir, "/all_good_series.csv"), header = T, stringsAsFactors = FALSE)
# clean_series <- unique(df_clean_series$series_id)
#clean_featureid <- as.numeric(gsub(",","", clean_featureid))

# get list of reviewed series to add
df_series <- tbl_series %>%
  dplyr::filter(reviewed == "true") %>%
  # dplyr::select(-flags) %>%
  dplyr::collect()

series_reviewed <- unique(df_series$series_id)
# 
# clean_series <- unique(c(clean_series, series_reviewed))

#--------------------- Remove locations with potential impoundment or tidal influence ---------------
df_impoundments <- read.csv(paste0(data_dir, "/impoundment_sites.csv"), header = TRUE, stringsAsFactors = FALSE)
df_tidal <- read.csv(paste0(data_dir, "/tidal_sites.csv"), header = TRUE, stringsAsFactors = FALSE)
exclude_locations <- unique(c(df_impoundments$id, df_tidal$id))

# df_exclude <- read.csv(paste0(data_dir, "/exclude_locations.csv"), header = TRUE, stringsAsFactors = FALSE)
# exclude_locations <- unique(df_exclude$id)

#------------------------fetch temperature data-------------------

start.time <- Sys.time()

df_values <- tbl(db, build_sql('SELECT * FROM "values_flags"')) %>%
  dplyr::left_join(tbl_series) %>%
  dplyr::left_join(tbl_variables) %>%
  dplyr::filter(variable_name == "TEMP",
                series_id %in% series_reviewed,
                flagged == FALSE) %>%
  dplyr::select(-file_id) %>%
  dplyr::collect(n = Inf) %>%
  dplyr::mutate(datetime=with_tz(datetime, tzone='EST'),
         date = as.Date(datetime),
         series_id = as.character(series_id)) %>%
  dplyr::rename(temp = value)

Sys.time() - start.time # ~ 36 minutes to get 56 million records

# clean locations
df_locations <- left_join(df_locations, df_agencies, by=c('agency_id'='agency_id')) %>%
  # filter(agency_name %in% keep_agencies) %>%
  dplyr::filter(agency_name != "TEST") %>%
  dplyr::rename(featureid=catchment_id)
str(df_locations)
summary(df_locations)
unique(df_locations$agency_name)


df_values <- df_values %>%
  dplyr::left_join(dplyr::select(df_locations, location_id, featureid, latitude, longitude, agency_name))
  
# summary(df_values)

df_values <- df_values %>%
  dplyr::filter(!is.na(featureid),
                !(location_id %in% exclude_locations))

saveRDS(df_values, file = file.path(getwd(), data_dir, "df_values.RData"))

# df_values <- readRDS(file = file.path(getwd(), data_dir, "df_values.RData"))



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

str(df_values)
summary(df_values)

# output flags table
sd_flags <- df_values %>%
  dplyr::filter(flag_incomplete == TRUE | 
                  flag_hourly_rise == TRUE |
                  flag_cold_obs == TRUE |
                  flag_hot_obs == TRUE |
                  flag_extremes == TRUE |
                  flag_interval == TRUE) %>%
  dplyr::select(-temp, -obs_per_day, -median_freq, -min_n90, -d_temp, -time_prev, -date)

write.csv(sd_flags, file = file.path(getwd(), data_dir, "subdaily_flags.csv"))

# split into sub-timeseries if there are any breaks in the timeseries greater than 1 day
# df_values <- as.data.frame(df_values)
# df_values$date_prev <- c(df_values[1,]$date, df_values[1:nrow(df_values)-1, ]$date)
# df_values[1, ]$date_prev <- NA # 2 steps to get correct date formatting
# df_values$series_prev <- c(NA, df_values[1:nrow(df_values)-1, ]$series_id)
# 
# df_values <- df_values %>%
#   dplyr::group_by(series_id) %>%
#   dplyr::mutate(new_series = ifelse(date > date_prev + 1 | series_id != series_prev, TRUE, FALSE))
# 
# df_values$new_series_id <- c(1, rep(NA, nrow(df_values)-1))
# system.time(
#   for(i in 1:nrow(df_values)) {
#   df_values$new_series_id[i] <- ifelse(df_values$new_series[i] == TRUE,
#                                     df_values$new_series_id[i-1] + 1,
#                                     df_values$new_series_id[i-1])
#   }
# )


df_values <- df_values %>%
  dplyr::group_by(series_id) 

# Median Absolute Deviations for spike detection

if(mad_tf == TRUE) {
df_values <- df_values %>%
  dplyr::mutate(MAD_normalized = MAD.roller(temp, median(median_freq)*10),
                MAD_normalized_30 = MAD.roller(temp, median(median_freq)*30),
                flag_MAD = ifelse(MAD_normalized > 10, TRUE, FALSE)) # 10 day moving window
}

save.image(file.path(getwd(), data_dir, "df_values_flags.RData"))

# plots for testing manually
if(mad_tf == TRUE) {
  
# identify sites with rate change problems and spikes
# ignore beginning, end, and winter for MAD because so many false positives
prob_rate_change <- df_values %>%
  dplyr::group_by(series_id) %>%
  dplyr::mutate(month = month(date)) %>%
  dplyr::filter(median_freq > 1,
                abs(d_temp) > 4 | MAD_normalized_30 > 5) %>%
  dplyr::filter(MAD_normalized != "Inf",
                month > 3 & month < 12) %>%
  dplyr::summarise(count = n())
length(unique(prob_rate_change$series_id))
length(unique(df_values$series_id))

# only pick up first or last value in spikes - try doing MAD iteratively after removing bad rate change and MAD points
# df_values <- df_values %>%
#   dplyr::filter(abs(d_temp) < 4 | MAD_normalized_30 < 10)
# 
# # Median Absolute Deviations for spike detection
# df_values <- df_values %>%
#   dplyr::group_by(series_id) %>%
#   dplyr::mutate(MAD_normalized = MAD.roller(temp, median(median_freq)*10),
#                 MAD_normalized_30 = MAD.roller(temp, median(median_freq)*30),
#                 flag_MAD = ifelse(MAD_normalized > 10, TRUE, FALSE)) # 10 day moving window
# 
# 
# # identify sites with rate change problems and spikes
# # ignore beginning, end, and winter for MAD because so many false positives
# prob_rate_change <- df_values %>%
#   dplyr::group_by(series_id) %>%
#   dplyr::mutate(month = month(date)) %>%
#   dplyr::filter(median_freq > 1,
#                 abs(d_temp) > 4 | MAD_normalized_30 > 5 | MAD_normalized > 5) %>%
#   dplyr::filter(MAD_normalized != "Inf",
#                 month > 3 & month < 12) %>%
#   dplyr::summarise(count = n())
# length(unique(prob_rate_change$series_id))
# length(unique(df_values$series_id))



# get list of bad series for plotting
bad_series_id <- unique(prob_rate_change$series_id)

length(bad_series_id)

if(!file.exists(paste0(data_dir, "/diagnostics/plots"))) dir.create(file.path(getwd(), data_dir, "diagnostics/plots"), recursive = TRUE)


for(i in 1:length(bad_series_id)) {
  bad_series <- as.data.frame(dplyr::filter(df_values, series_id == bad_series_id[i]))
  bad_hourly <- bad_series %>%
    dplyr::filter(abs(d_temp) > 4) %>%
    as.data.frame(stringsAsFactors = FALSE)
  bad_MAD <- bad_series %>%
    dplyr::filter(MAD_normalized > 10) %>%
    as.data.frame(stringsAsFactors = FALSE)
  bad_MAD_30 <- bad_series %>%
    dplyr::filter(MAD_normalized_30 > 4) %>%
    as.data.frame(stringsAsFactors = FALSE)
  g <- ggplot(bad_series, aes(datetime, temp)) + geom_line() + ylim(c(0, 30)) + ggtitle(paste0("series_id: ", bad_series_id[i]))
  if(nrow(bad_MAD_30) > 0) {
    g <- g + geom_point(data = bad_MAD_30, aes(datetime, temp), colour = "yellow", size = 5, alpha = 0.5)
  }
  g <- g + geom_point(aes(colour = MAD_normalized)) + scale_colour_gradient(high = "red", low = "black", limits = c(0, 4)) + theme_bw()
  ggsave(paste0(data_dir, "/diagnostics/plots/series_", bad_series_id[i], ".png"), plot = g)
}

} # end MAD check




# Filter based on subdaily flags
df_values2 <- df_values %>%
  dplyr::group_by(series_id, date) %>%
  dplyr::filter(flag_incomplete == "FALSE", # make all != TRUE to include NA?
         flag_cold_obs == "FALSE",
         flag_hot_obs == "FALSE",
         flag_interval == "FALSE" | is.na(flag_interval),
         abs(d_temp) < 5 | is.na(d_temp))
         
if(mad_tf == TRUE) {
  df_values2 <- df_values2 %>%
    dplyr::filter(MAD_normalized < 5)
}


# examine vs. auto filter hourly_rise?


# Convert to daily
df_values3 <- df_values2 %>%
  dplyr::group_by(series_id, date, location_id, agency_id) %>%
  dplyr::filter(flagged == "FALSE") %>%
  dplyr::summarise(meanTemp = mean(temp), 
            maxTemp = max(temp), 
            minTemp = min(temp), 
            #obs_per_day = mean(obs_per_day),  # change mean to median when dplyr fixed
            n_obs = n()) %>%
  dplyr::rename(temp = meanTemp)
summary(df_values3)
dim(df_values3)

######## QAQC on daily#######

df_values3 <- df_values3 %>%
  flag_daily_rise(.) %>%
  flag_cold_days(.) %>%
  flag_hot_days(.) %>%
  flag_extreme_days(.)


# output daily flags
d_flags <- df_values3 %>%
  dplyr::filter(flag_daily_rise == TRUE |
                  flag_cold_days == TRUE |
                  flag_hot_days == TRUE |
                  flag_extreme_days == TRUE) %>%
  dplyr::select(-row, -temp_prev, -series_prev, -series_start, -median_freq, -min_n90)

df_values3 <- df_values3 %>%
  dplyr::group_by(series_id)
  
# Median Absolute Deviations for spike detection
if(mad_tf == TRUE) {
df_values3 <- df_values3 %>%
  dplyr::mutate(MAD_normalized = MAD.roller(temp, median(median_freq)*10)) #,
                #MAD_normalized_30 = MAD.roller(temp, median(median_freq)*30),
                #flag_MAD = ifelse(MAD_normalized > 10, TRUE, FALSE)) # 10 day moving window
}

write.csv(d_flags, file = file.path(getwd(), data_dir, "daily_flags.csv"))

# filter for use in the model
df_values3 <- df_values3 %>%
  dplyr::filter(flag_cold_days == "FALSE",
                abs(d_temp) < 10 | is.na(d_temp))

if(mad_tf == TRUE) {
  df_values3 <- df_values3 %>%
    dplyr::filter(MAD_normalized < 8)
}

# End QAQC

#------------ get location and agency data ----------
# Get location and agency data and join to temperature data
# df_locations <- collect(select(tbl_locations, location_id, location_name, latitude, longitude, featureid=catchment_id))
temperatureData <- df_values3 %>%
  dplyr::left_join(df_locations) %>%
  #left_join(df_agencies, by = 'agency_id') %>%
  dplyr::select(series_id, location_id, agency_name, location_name, latitude, longitude, featureid, date, temp, maxTemp, minTemp, n_obs) %>%
  dplyr::mutate(agency_name=factor(agency_name),
         location_name=factor(location_name),
         year = year(date))

# create temperatureData input dataset
summary(temperatureData)

# save series used
df_series <- data.frame(series_id = unique(df_values3$series_id), stringsAsFactors = FALSE)
write.csv(df_series, file = paste0(data_dir, "/series_used.csv"), row.names = FALSE)


#Summarise to featureid
# for all current analyses just need mean within reach (featureid)

# does mean make sense because could result in discontinuities in time series within a reach if one logger was in a seep and other not

# Need to filter by agency and such by here because can't do it later if multiple agencies in the same reach (featureid)
temperatureData2 <- temperatureData %>%
  dplyr::group_by(featureid, year, date) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T), 
                   tempMax = mean(maxTemp, na.rm = T),
                   tempMin = mean(minTemp, na.rm = T),
                   n_per_day = sum(n_obs))

temperatureData2 <- temperatureData2 %>%
  dplyr::mutate(temp = ifelse(temp < 0, 0, temp),
                tempMax = ifelse(tempMax < 0, 0, tempMax),
                tempMin = ifelse(tempMin < 0, 0, tempMin))
summary(temperatureData2)

years <- unique(temperatureData2$year)
featureids <- unique(temperatureData2$featureid)
featureids <- featureids[!is.na(featureids)]

closeAllConnections()
rm(db)
gc()

save.image(paste0(data_dir, "/temp_temp.RData"))

#-----------------------fetch covariates-----------------------
featureids <- as.integer(featureids)

# connect to database source
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

# fetch covariates
# featureid |  variable  | value | zone  | riparian_distance_ft 
cov_fetch <- c("agriculture", "alloffnet", "allonnet", "AreaSqKM", "devel_hi", "devel_low", "devel_med", "developed", "devel_opn", "drainageclass", "elevation", "forest", "fwsopenwater", "fwswetlands", "herbaceous", "hydrogroup_a", "hydrogroup_ab", "hydrogroup_cd", "hydrogroup_d1", "hydrogroup_d4", "impervious", "openoffnet", "openonnet", "percent_sandy", "slope_pcnt", "surfcoarse", "tree_canopy", "undev_forest", "water", "wetland")

start.time <- Sys.time()
tbl_covariates <- tbl(db, 'covariates') %>%
  dplyr::filter(featureid %in% featureids & variable %in% cov_fetch)

df_covariates_long <- dplyr::collect(tbl_covariates)

Sys.time() - start.time

df_covariates <- df_covariates_long %>%
  tidyr::spread(variable, value) # convert from long to wide by variable
summary(df_covariates)

# need to organize covariates into upstream or local by featureid
upstream <- df_covariates %>%
  dplyr::group_by(featureid) %>%
  dplyr::filter(zone == "upstream",
                is.na(riparian_distance_ft)) %>%
  # dplyr::select(-zone, -location_id, -location_name) %>%
  # dplyr::summarise_each(funs(mean)) %>% # needed???
  dplyr::rename(forest_all = forest)

# Get upstream riparian forest
riparian_200 <- df_covariates %>%
  dplyr::group_by(featureid) %>%
  dplyr::select(featureid, forest, zone, riparian_distance_ft) %>%
  dplyr::filter(zone == "upstream",
                riparian_distance_ft == 200)

# create covariateData input dataset
covariateData <- riparian_200 %>%
  dplyr::select(-riparian_distance_ft) %>%
  dplyr::left_join(upstream)


# covariateData <- left_join(covariateData,
#                            dplyr::select(df_locations, location_id, location_name, latitude, longitude, featureid)) %>%
#   dplyr::mutate(location_name=factor(location_name))
# summary(covariateData)


#---------------------daymet climate data-------------------------

# too big/slow to pull through R so make the query and export that. The resulting sql script can then be run via command line or within a bash script or make file

# reduce featureid list to those with drainage areas <=200 sq km
df_featureid_200 <- covariateData %>%
  dplyr::filter(AreaSqKM <= 200) %>%
  dplyr::select(featureid)

featureids <- unique(df_featureid_200$featureid)

featureids_string <- paste(featureids, collapse=', ')
years_string <- paste(years, collapse=', ')

qry <- paste0("COPY(SELECT featureid, date_part('year', date) as year, date, tmax, tmin, prcp, dayl, srad, swe FROM daymet WHERE featureid IN (", featureids_string, ") AND date_part('year', date) IN (",years_string, ") ) TO STDOUT CSV HEADER;")  #

if(!file.exists(file.path(getwd(), data_dir, "code"))) dir.create(file.path(getwd(), data_dir, "code"))
cat(qry, file = paste0(data_dir, "/code/daymet_query.sql"))


#----------------save files---------------------

saveRDS(temperatureData2, file=output_file1)
#saveRDS(upstream, file=output_file2)
saveRDS(covariateData, file=output_file2)
#saveRDS(data_dir, file=)
#saveRDS(climateData, file=output_file3)

library(foreign)
write.dbf(data.frame(df_featureid_200, stringsAsFactors = FALSE), file = paste0(data_dir, "/featureid_list_20160602.dbf"))

#---------------cleaning---------------------



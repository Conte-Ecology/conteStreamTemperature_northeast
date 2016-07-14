# Prepare model input dataset (tempDataSync)
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript breakpoints.R <input temperatureData rdata> <input climateData csv> <input springFallBPs rdata> <output tempDataSync rdata>
# example: $ Rscript prepare_model_data.R ./temperatureData.RData ./daymet_results.csv ./springFallBPs.RData ./tempDataSync.RData
#
# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(ggmcmc) # load before dplyr so later plyr doesn't override dplyr
library(dplyr)
library(DataCombine) # for the slide function
library(zoo) # for rollapply function
library(lubridate)
library(stringr)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(jsonlite)
library(foreign)
library(readr)

config <- fromJSON('model_config.json') # consider this as an input to the run_model.sh script

# validate = TRUE # get rid of this once model_config.json is ready

# get current model run directory
data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1])

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# until running as a bash script add the files here
if(length(args) < 1) {
  args <- c(paste0(data_dir, "/temperatureData.RData"), paste0(data_dir, "/daymet_results.csv"), paste0(data_dir, "/covariateData.RData"), paste0(data_dir, "/springFallBPs.RData"), paste0(data_dir, "/tempDataSync.RData")) # paste0(data_dir, "/sample_locations_50m.csv")
}

temperatureData_file <- args[1]
if (!file.exists(temperatureData_file)) {
  stop(paste0('Could not find temperatureData binary file: ', temperatureData_file))
}
temperatureData <- readRDS(temperatureData_file)

climateData_file <- args[2]
if (!file.exists(climateData_file)) {
  stop(paste0('Could not find climateData binary file: ', climateData_file))
}
climateData <- fread(climateData_file, header = TRUE, sep = ",")

covariateData_file <- args[3]
if (!file.exists(covariateData_file)) {
  stop(paste0('Could not find covariateData binary file: ', covariateData_file))
}
covariateData <- readRDS(covariateData_file)

springFallBPs_file <- args[4]
if (!file.exists(springFallBPs_file)) {
  stop(paste0('Could not find springFallBPs binary file: ', springFallBPs_file))
}
springFallBPs <- readRDS(springFallBPs_file)

output_file <- args[5]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

tempData <- climateData %>%
  dplyr::mutate(site = as.numeric(as.factor((featureid))),
                dOY = yday(date),
                airTemp = (tmax + tmin) / 2)

# Order by group and date
tempData <- tempData[order(tempData$site,tempData$year,tempData$dOY), ]

# For checking the order of tempDataSync
tempData$count <- 1:length(tempData$year)

tempData <- tempData[order(tempData$count),] # just to make sure tempDataSync is ordered for the slide function

# moving means instead of lagged terms in the future
tempData <- tempData %>%
  group_by(site, year) %>%
  arrange(site, year, dOY) %>%
  mutate(airTempLagged1 = lag(airTemp, n = 1, fill = NA),
         #airTempLagged2 = lag(airTemp, n = 2, fill = NA),
         #prcpLagged1 = lag(prcp, n = 1, fill = NA),
         #prcpLagged2 = lag(prcp, n = 2, fill = NA),
         #prcpLagged3 = lag(prcp, n = 3, fill = NA),
         #temp5 = rollsum(x = airTemp, 5, align = "right", fill = NA),
         temp5p = rollapply(data = airTempLagged1, 
                            width = 5, 
                            FUN = mean, 
                            align = "right", 
                            fill = NA, 
                            na.rm = T),
         temp7p = rollapply(data = airTempLagged1, 
                            width = 7, 
                            FUN = mean, 
                            align = "right", 
                            fill = NA, 
                            na.rm = T),
         prcp2 = rollsum(x = prcp, 2, align = "right", fill = NA),
         prcp7 = rollsum(x = prcp, 7, align = "right", fill = NA),
         prcp30 = rollsum(x = prcp, 30, align = "right", fill = NA))

tempDataBP <- temperatureData %>%
  dplyr::filter(!is.na(featureid)) %>%
  left_join(dplyr::mutate(data.frame(unclass(tempData)), date = as.Date(date))) %>%
  left_join(covariateData, by = c("featureid")) %>%
  dplyr::mutate(site = as.character(featureid),
                impoundArea = AreaSqKM * allonnet / 100)

#-------------Breakpoints ----------------------------

springFallBPs$site <- as.character(springFallBPs$site)

# Join with break points
tempDataBP <- left_join(tempDataBP, springFallBPs)

# highly buffered sites (springs) and highly variable sites have trouble with the breakpoints. Make day 90 the earliest possible and 330 the latest possible
tempDataBP <- tempDataBP %>%
  dplyr::mutate(finalSpringBP = ifelse(finalSpringBP < 75.0 | is.na(finalSpringBP), 75.0, finalSpringBP),
                finalFallBP = ifelse(finalFallBP > 330.0 | is.na(finalFallBP), 330.0, finalFallBP)) # adjust to take mean by site, huc12, 10, 8, 4, grand mean, min 75

#------------ evaluate break points ---------------
bp_test <- FALSE
if(bp_test) {
  df <- tempDataBP %>%
    dplyr::select(featureid, year, date, dOY, temp, airTemp, finalSpringBP, finalFallBP, sourceSpringBP, sourceFallBP) 
  
  ggplot(df, aes(finalSpringBP)) + geom_histogram()
  ggplot(df, aes(finalFallBP)) + geom_histogram()
  
  foo <- df %>%
    dplyr::filter(finalSpringBP < 90 | finalFallBP >= 340)
  
  str(foo)
  unique(foo$featureid)
  
  for(i in 1:length(unique(foo$featureid))) {
    bar <- dplyr::filter(foo, featureid == unique(foo$featureid)[i])
    g <- ggplot(bar, aes(dOY, temp)) + geom_point(colour = "blue") + geom_line(colour = "blue") + geom_point(aes(dOY, airTemp))  + geom_vline(aes(xintercept = bar$finalFallBP[1]), colour = "red") + geom_vline(aes(xintercept = bar$finalSpringBP[1]), colour = "red")
    print(g)
  } # all look reasonable
  
} # end if statement for BP_test
#---------------------------------

# Clip to syncronized season
tempDataSync <- dplyr::filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

# Filter by Drainage area
tempDataSync <- tempDataSync %>%
  dplyr::filter(AreaSqKM >= 0.01 & AreaSqKM < 200 & allonnet < 50) %>%
  dplyr::filter(!is.na(impoundArea)) %>%
  dplyr::mutate(featureid_year = paste0(featureid, "_", year))# %>%
# dplyr::filter(!(featureid %in% exclude_ids))

#---------------------------------------------------------------------------------------

var.names <- c("airTemp", 
               #"airTempLagged1", 
               #"airTempLagged2",
               "temp7p",
               "prcp", 
               "prcp2",
               "prcp7",
               "prcp30",
               #"prcpLagged1", 
               #"prcpLagged2", 
               #"prcpLagged3", 
               "dOY", 
               "forest", 
               "herbaceous", 
               "agriculture", 
               "devel_hi", 
               "developed",
               "AreaSqKM",  
               "allonnet",
               "alloffnet",
               "impoundArea",
               "surfcoarse", 
               "srad", 
               "dayl", 
               "swe")

featureids <- as.integer(unique(tempDataSync$featureid))

# connect to database source
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_huc12 <- tbl(db, 'catchment_huc12') %>%
  dplyr::filter(featureid %in% featureids) %>%
  dplyr::mutate(HUC4=substr(huc12, as.integer(1), as.integer(4)),
                HUC8=substr(huc12, as.integer(1), as.integer(8)),
                HUC10=substr(huc12, as.integer(1), as.integer(10)))

df_huc <- tbl_huc12 %>%
  dplyr::collect(n = Inf) %>%
  dplyr::rename(HUC12 = huc12)

tempDataSync <- tempDataSync %>%
  dplyr::left_join(df_huc)

tempDataSync <- tempDataSync %>%
  dplyr::filter(!is.na(prcp),
                !is.na(prcp30),
                !is.na(airTemp),
                !is.na(agriculture),
                !is.na(alloffnet),
                !is.na(surfcoarse))

#------------- cut first and last day of each time series ----------------------
tempDataSync <- indexDeployments(tempDataSync, regional = TRUE)
firstObsRows <- createFirstRows(tempDataSync)
lastObsRows <- tempDataSync %>% 
  group_by(deployID) %>% 
  filter(date == max(date) | is.na(temp)) %>% 
  select(rowNum)

tempDataSync <- tempDataSync %>%
  dplyr::filter(!(rowNum %in% firstObsRows),
                !(rowNum %in% lastObsRows))

# second filter to reduce second and second to last day in each time series
tempDataSync <- tempDataSync %>% 
  group_by(deployID) %>% 
  filter(date != max(date),
         date != min(date))

#----------- exclude unnaturally warm sites ------------------
# sites may be warm due to impoundments, effluent, or loggers in the sun (frequently warmer than the air temp)
# exclude excessively warm sites (due to impoundments, effluent, sun, etc.)
df_warm <- flag_warm_influence(data = dplyr::group_by(tempDataSync, featureid_year), vals = "temp", refs = "airTemp", threshold = 0.85)

tempDataSync <- tempDataSync %>%
  dplyr::group_by(featureid_year) %>%
  dplyr::mutate(MAD_normalized = MAD.roller(temp, 6),
                MAD_normalized_10 = MAD.roller(temp, 10),
                MAD_normalized_30 = MAD.roller(temp, 30),
                flag_MAD = ifelse(MAD_normalized > 10, TRUE, FALSE))

series <- unique(df_warm[which(df_warm$prop_warm >0.75 & df_warm$prop_warm < 0.85), ]$featureid_year)
n <- length(series)
dir.create(path = file.path(data_dir, "warm_sites"))

for(i in 1:n) {
  data_1 <- dplyr::filter(tempDataSync, featureid_year == series[i])
  g <- ggplot(data_1, aes(date, temp)) + ylim(c(0, 35)) + ggtitle(paste0("featureid: ", data_1$featureid[1], " | year: ", data_1$year[1], " | prop_warm: ", round(df_warm[which(df_warm$featureid_year == series[i]), ]$prop_warm, digits = 2))) #+ facet_wrap(~year)
  #   if(nrow(bad_MAD_30) > 0) {
  #     g <- g + geom_point(data = bad_MAD_30, aes(datetime, temp), colour = "yellow", size = 5, alpha = 0.5)
  #   }
  g <- g + geom_point(aes(colour = MAD_normalized_10)) + geom_line(aes(colour = MAD_normalized_10)) + 
    scale_colour_gradient(high = "yellow", low = "blue", limits = c(0, 6)) 
  
  # add agency name
  g <- g +
    geom_line(aes(date, airTemp), colour = "black") +
    #geom_line(aes(date, tmax), colour = "gray") +
    #geom_line(aes(date, tmin), colour = "gray") +
    #geom_line(aes(date, trend), colour = "red") + 
    xlab("Date") + 
    ylab("Temperature (C)") +
    theme_bw()
  ggsave(paste0(data_dir, "/warm_sites/", series[i], ".png"), plot = g)
}

#bad_featureid_year <- c("738125_2012", "750893_2010", "750978_2008", "750978_2010", "777209_2013", "757508_2005", "467429_2005", "738115_2012", "870984_2011", "870984_2011")
tempDataSync <- tempDataSync %>%
  dplyr::left_join(df_warm) %>%
  dplyr::filter(flag_warm == FALSE) %>% #,
  # !(featureid_year %in% bad_featureid_year)) %>%
  dplyr::select(-flag_warm, -featureid_year) 

length(unique(temperatureData$featureid))
length(unique(tempDataSync$featureid))
#length(unique(tempDataBP2$featureid))

#-------------- remove site-year combos with fewer than 4 data points remaining --------
tempDataSync$huc <- tempDataSync$HUC8

keep_columns <- c("featureid"
                  , "featureid_year"
                  , "date"
                  , "year"
                  , "airTemp"
                  , "temp"
                  , "tempMax"
                  , "tempMin"
                  #, "n_obs"
                  , "tmax"
                  , "tmin"
                  , "prcp"
                  , "dayl"
                  , "srad"
                  , "swe"
                  , "site"
                  , "dOY"
                  , "airTemp"
                  , "temp7p"
                  #, "count"
                  , "prcp2"
                  , "prcp7"
                  , "prcp30"
                  #, "latitude"
                  # , "longitude"
                  , "agriculture"
                  , "herbaceous"
                  , "allonnet"
                  , "alloffnet"
                  , "AreaSqKM"
                  , "impoundArea"
                  , "developed"
                  , "devel_hi"
                  , "elevation"
                  , "forest"
                  , "impervious"
                  , "openonnet"
                  , "percent_sandy"
                  , "surfcoarse"
                  , "finalSpringBP"
                  , "finalFallBP"
                  , "tree_canopy"
                  , "HUC4"
                  , "HUC8"
                  , "HUC10"
                  , "HUC12"
                  , "huc")

tempDataSync <- dplyr::select(tempDataSync, one_of(keep_columns))

tempDataSync <- as.data.frame(unclass(tempDataSync), stringsAsFactors = FALSE)

### Separate data for fitting (training) and validation
#If validating:
if (config[['validate']]) {
  validate.frac.site.years <- 0.10
  validate.frac.sites <- 0.10
  #validate.frac.huc <- 0.05
  
  # leave out sites and hucs at random
  n.fit.site.years <- floor(length(unique(tempDataSync$featureid_year)) * (1 - validate.frac.site.years))
  n.fit.sites <- floor(length(unique(tempDataSync$site)) * (1 - validate.frac.sites))
  # n.fit.hucs <- floor(length(unique(tempDataSync$huc)) * (1 - validate.frac.huc))
  
  # leave out especially warm year (summer) across the regional
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(airTemp.mean = mean(airTemp))
  ggplot(foo, aes(year, airTemp.mean)) + geom_point() + geom_smooth() # 1999, 2005, 2010 all high tmean summers
  
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(tmax.mean = mean(tmax))
  ggplot(foo, aes(year, tmax.mean)) + geom_point() + geom_smooth() # 1995, 1999, 2005, 2010 all high tmax summers
  
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(tmin.mean = mean(tmin))
  ggplot(foo, aes(year, tmin.mean)) + geom_point() + geom_smooth() # 2005, 2006, 2010 all high tmin summers
  
  
  # random TO KEEP
  
  ##### add site_year combos for sites with multiple years and maybe part timeseries (half a time series or month randomly left out)
  
  set.seed(23461346)
  site.year.fit <- sample(unique(tempDataSync$featureid_year), n.fit.site.years, replace = FALSE)
  site.fit <- sample(unique(tempDataSync$site), n.fit.sites, replace = FALSE) # select sites to hold back 
  year.valid <- "2010"
  
  tempDataSyncValid <- tempDataSync %>%
    dplyr::filter(!(featureid_year %in% site.year.fit) | !(site %in% site.fit) | year == year.valid) # 
  
  tempDataSync <- tempDataSync %>%
    dplyr::filter(featureid_year %in% site.year.fit & site %in% site.fit & year != year.valid)   # data for model fitting (calibration) & huc %in% huc.fit
  
  print(paste0(round(nrow(tempDataSyncValid)/(nrow(tempDataSync) + nrow(tempDataSyncValid))*100, digits = 1), "% of data points held out for validation"))
  
  
  #tempDataSyncValidS <- stdCovs(x = tempDataSyncValid, y = tempDataSync, var.names = var.names)
  
  ########## Means and SDs for Standardization ###########
  means <- NULL
  stdevs <- NULL
  for(i in 1:length(var.names)) {
    means[i] <- mean(tempDataSync[, var.names[i]], na.rm = T)
    stdevs[i] <- sd(tempDataSync[, var.names[i]], na.rm = T)
  }
  df_stds <- data.frame(var.names, means, stdevs, stringsAsFactors = FALSE)
  
  tempDataSyncValidS <- stdCovs(x = tempDataSyncValid, y = df_stds, var.names = var.names)
  #############
  
  tempDataSyncValidS <- indexDeployments(tempDataSyncValidS, regional = TRUE)
  firstObsRowsValid <- createFirstRows(tempDataSyncValidS)
  evalRowsValid <-createEvalRows(tempDataSyncValidS)
  tempDataSyncValidS <- addInteractions(tempDataSyncValidS)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  df_site <- data.frame(site = unique(tempDataSyncS$featureid))
  df_site$sitef <- seq(1, nrow(df_site), by = 1)
  df_huc <- data.frame(huc = unique(tempDataSyncS$huc))
  df_huc$hucf <- seq(1, nrow(df_huc), by = 1)
  df_year <- data.frame(year = unique(tempDataSyncS$year))
  df_year$yearf <- seq(1, nrow(df_year), by = 1)
  J <- nrow(df_site)
  M <- nrow(df_huc)
  Ti <- nrow(df_year)
  
  rand_ids <- list(df_site = df_site, df_huc = df_huc, df_year = df_year, J = J, M = M, Ti = Ti)
  
  rand_ids$df_site$site <- as.character(rand_ids$df_site$site)
  rand_ids$df_huc$huc <- as.character(rand_ids$df_huc$huc)
  
  tempDataSyncS <- tempDataSyncS %>%
    dplyr::select(-sitef) %>%
    dplyr::left_join(rand_ids$df_site) %>%
    dplyr::left_join(rand_ids$df_huc) %>%
    dplyr::left_join(rand_ids$df_year)
  
  save(tempDataSync, tempDataSyncS, tempDataSyncValid, tempDataSyncValidS, firstObsRows, evalRows, firstObsRowsValid, evalRowsValid, var.names, df_stds, rand_ids, file = output_file)
  
  # export locations with calibration or validation
  series_used <- read.csv(paste0(data_dir, "/series_used.csv"), header = TRUE, stringsAsFactors = FALSE)
  
  db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
  tbl_locations <- tbl(db, 'locations') %>%
    dplyr::rename(location_id=id, location_name=name) %>%
    dplyr::select(-created_at, -updated_at, -description)
  
  tbl_agencies <- tbl(db, 'agencies') %>%
    dplyr::rename(agency_id=id, agency_name=name) %>%
    dplyr::select(-created_at, -updated_at, -description)
  
  df_series <- tbl(db, 'series') %>%
    dplyr::rename(series_id=id) %>%
    dplyr::select(-created_at, -updated_at, -flags) %>%
    dplyr::filter(reviewed == "true") %>%
    dplyr::left_join(tbl_locations) %>%
    dplyr::left_join(tbl_agencies) %>%
    # dplyr::select(-flags) %>%
    dplyr::select(-variable_id, -reviewed) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::filter(series_id %in% unique(series_used$series_id))
  
  df_location_summary <- df_series %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(featureid = mean(catchment_id),
                     latitude = mean(latitude),
                     longitude = mean(longitude),
                     num_records = sum(value_count),
                     agency_id = mean(agency_id))
  
  closeAllConnections()
  
  # determine what featureid where used for validation and calibration
  featureid_valid <- data.frame(featureid = setdiff(unique(tempDataSyncValidS$featureid), unique(tempDataSyncS$featureid)), use = "validation", stringsAsFactors = FALSE)
  featureid_fit <- data.frame(featureid = setdiff(unique(tempDataSyncS$featureid), unique(tempDataSyncValidS$featureid)), use = "calibration", stringsAsFactors = FALSE) 
  featureid_both <- data.frame(featureid = intersect(unique(tempDataSyncS$featureid), unique(tempDataSyncValidS$featureid)), use = "both", stringsAsFactors = FALSE)
  
  featureid_use <- dplyr::bind_rows(featureid_valid, featureid_fit, featureid_both)
  
  # combine locations and use in model by featureid
  df_locations_used <- df_location_summary %>%
    dplyr::left_join(featureid_use)
  
  # write out
  write_csv(df_locations_used, paste0(data_dir, "/location_use.csv"))
  write.dbf(as.data.frame(df_locations_used, stringsAsFactors = FALSE), paste0(data_dir, "/location_use.dbf"))
  
} else {
  means <- NULL
  stdevs <- NULL
  for(i in 1:length(var.names)) {
    means[i] <- mean(tempDataSync[, var.names[i]], na.rm = T)
    stdevs[i] <- sd(tempDataSync[, var.names[i]], na.rm = T)
  }
  
  df_stds <- data.frame(var.names, means, var.names, stdevs, stringsAsFactors = FALSE)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  df_site <- data.frame(site = unique(tempDataSyncS$featureid))
  df_site$sitef <- seq(1, nrow(df_site), by = 1) # only works for data to fit, not data to predict
  df_huc <- data.frame(huc = as.character(unique(tempDataSyncS$huc)))
  df_huc$hucf <- seq(1, nrow(df_huc), by = 1)
  df_year <- data.frame(year = unique(tempDataSyncS$year))
  df_year$yearf <- seq(1, nrow(df_year), by = 1)
  J <- nrow(df_site)
  M <- nrow(df_huc)
  Ti <- nrow(df_year)
  
  rand_ids <- list(df_site = df_site, df_huc = df_huc, df_year = df_year, J = J, M = M, Ti = Ti)
  
  rand_ids$df_site$site <- as.character(rand_ids$df_site$site)
  rand_ids$df_huc$huc <- as.character(rand_ids$df_huc$huc)
  
  tempDataSyncS <- tempDataSyncS %>%
    dplyr::select(-sitef) %>%
    dplyr::left_join(rand_ids$df_site) %>%
    dplyr::left_join(rand_ids$df_huc) %>%
    dplyr::left_join(rand_ids$df_year)
  
  save(tempDataSync, tempDataSyncS, firstObsRows, evalRows, var.names, df_stds, rand_ids, file = output_file)
  
  # export locations with calibration or validation
  series_used <- read.csv(paste0(data_dir, "/series_used.csv"), header = TRUE, stringsAsFactors = FALSE)
  
  db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
  tbl_locations <- tbl(db, 'locations') %>%
    dplyr::rename(location_id=id, location_name=name) %>%
    dplyr::select(-created_at, -updated_at, -description)
  
  tbl_agencies <- tbl(db, 'agencies') %>%
    dplyr::rename(agency_id=id, agency_name=name) %>%
    dplyr::select(-created_at, -updated_at, -description)
  
  df_series <- tbl(db, 'series') %>%
    dplyr::rename(series_id=id) %>%
    dplyr::select(-created_at, -updated_at, -flags) %>%
    dplyr::filter(reviewed == "true") %>%
    dplyr::left_join(tbl_locations) %>%
    dplyr::left_join(tbl_agencies) %>%
    # dplyr::select(-flags) %>%
    dplyr::select(-variable_id, -reviewed) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::filter(series_id %in% unique(series_used$series_id))
  
  df_location_summary <- df_series %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(featureid = mean(catchment_id),
                     latitude = mean(latitude),
                     longitude = mean(longitude),
                     num_records = sum(value_count),
                     agency_id = mean(agency_id))
  
  closeAllConnections()
  
  # determine what featureid where used for validation and calibration
  featureid_fit <- data.frame(featureid = setdiff(unique(tempDataSyncS$featureid), unique(tempDataSyncValidS$featureid)), use = "calibration", stringsAsFactors = FALSE)
  featureid_use <- featureid_fit
  
  # combine locations and use in model by featureid
  df_locations_used <- df_location_summary %>%
    dplyr::left_join(featureid_use)
  
  # write out
  library(foreign)
  library(readr)
  write_csv(df_locations_used, paste0(data_dir, "/location_use.csv"))
  write.dbf(as.data.frame(df_locations_used, stringsAsFactors = FALSE), paste0(data_dir, "/location_use.dbf"))
  
}

# Prepare model input dataset (tempDataSync)
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript breakpoints.R <input temperatureData rdata> <input climateData csv> <input springFallBPs rdata> <output tempDataSync rdata>
# example: $ Rscript prepare_model_data.R ./temperatureData.RData ./daymet_results.csv ./springFallBPs.RData ./tempDataSync.RData
#
# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

library(data.table)
library(ggplot2)
library(ggmcmc) # load before dplyr so later plyr doesn't override dplyr
library(dplyr)
library(DataCombine) # for the slide function
library(lubridate)
library(devtools)
# install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(jsonlite)

config <- fromJSON('model_config.json')

# validate = TRUE # get rid of this once model_config.json is ready

data_dir <- "localData_2015-06-09" 

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# until running as a bash script add the files here
if(length(args) < 1) {
  args <- c(paste0(data_dir, "/temperatureData.RData"), paste0(data_dir, "/daymet_results.csv"), paste0(data_dir, "/covariateData.RData"), paste0(data_dir, "/springFallBPs.RData"), paste0(data_dir, "/tempDataSync.RData"))
}


######### add wd argument

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

# airTemp
#tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
#tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')

# prcp
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')

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


# masterData <- masterData[, c("agency", "date", "AgencyID", "year", "site", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")]

#tempData <- left_join(tempData, covariateData)


#covariateData <- readStreamTempData(timeSeries=TRUE, covariates=TRUE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

tempDataBP <- temperatureData %>%
  left_join(dplyr::mutate(data.frame(unclass(tempData)), date = as.Date(date))) %>%
  left_join(covariateData, by = c("featureid")) %>%
  dplyr::mutate(site = as.character(featureid))

springFallBPs$site <- as.character(springFallBPs$site)

# Join with break points
tempDataBP <- left_join(tempDataBP, springFallBPs)

#str(tempDataBP)
#str(tempDataSync)

# Clip to syncronized season
tempDataSync <- dplyr::filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

# Filter by Drainage area
tempDataSync <- tempDataSync %>%
  filter(AreaSqKM  < 400)


#tempDataSync <- left_join(tempDataSync, covariateData)

#tempDataSync <- tempDataSync[ , c("agency", "date", "AgencyID", "year", "site", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")]

# consider limiting to small-medium watersheds - add to model_config.json
#if(class(filter.area) == "numeric") tempDataSync <- filter(tempDataSync, filter = TotDASqKM <= filter.area)

#tempDataSync <- na.omit(tempDataSync) ####### Needed to take out first few days that get NA in the lagged terms. Change this so don't take out NA in stream temperature?

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
               "surfcoarse", 
               "srad", 
               "dayl", 
               "swe")

# Get HUC8

featureids <- unique(tempDataSync$featureid)

# connect to database source
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_huc12 <- tbl(db, 'catchment_huc12') %>%
  dplyr::filter(featureid %in% featureids)

df_huc <- tbl_huc12 %>%
  dplyr::collect() %>%
  dplyr::mutate(HUC4=as.character(str_sub(huc12, 1, 4)),
                HUC8=as.character(str_sub(huc12, 1, 8)),
                HUC10=as.character(str_sub(huc12, 1, 10)),
                huc = as.character(HUC8)) %>%
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
#dplyr::select()


keep_columns <- c("featureid"
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
                  , "latitude"
                  , "longitude"
                  , "agriculture"
                  , "herbaceous"
                  , "allonnet"
                  , "alloffnet"
                  , "AreaSqKM"
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
  validate.frac.sites <- 0.20
  validate.frac.huc <- 0.10
  
  # leave out sites and hucs at random
  n.fit.sites <- floor(length(unique(tempDataSync$site)) * (1 - validate.frac.sites))
  n.fit.hucs <- floor(length(unique(tempDataSync$HUC8)) * (1 - validate.frac.huc))
  
  # leave out 2010 because regional warm year
  
  # random TO KEEP
  set.seed(2346)
  site.fit <- sample(unique(tempDataSync$site), n.fit.sites, replace = FALSE) # select sites to hold back for testing 
  huc.fit <- sample(unique(tempDataSync$HUC8), n.fit.hucs, replace = FALSE) # select hucs to hold back for testing 
  year.valid <- "2010"
  
  tempDataSyncValid <- tempDataSync %>%
    dplyr::filter(!(site %in% site.fit) | !(HUC8 %in% huc.fit) | year == year.valid) # data for validation
  
  #featureid_date <- paste0(tempDataSync$featureid, "_", tempDataSync$date)
  #valid_set <- unique(tempDataSyncValid$featureid)
  
  tempDataSync <- tempDataSync %>%
    dplyr::filter(site %in% site.fit & HUC8 %in% huc.fit & year != year.valid)   # data for model fitting (calibration)
  
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
  
  
  
  save(tempDataSync, tempDataSyncS, tempDataSyncValid, tempDataSyncValidS, firstObsRows, evalRows, firstObsRowsValid, evalRowsValid, df_stds, file = output_file)
  
} else {
  #tempDataSyncValid <- NULL
  
  means <- NULL
  stdevs <- NULL
  for(i in 1:length(var.names)) {
    means[i] <- mean(tempDataSync[, var.names[i]], na.rm = T)
    stdevs[i] <- sd(tempDataSync[, var.names[i]], na.rm = T)
  }
  
  df_stds <- data.frame(var.names, means, stdevs, stringsAsFactors = FALSE)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  save(tempDataSync, tempDataSyncS, firstObsRows, evalRows, df_stds, file = output_file)
}

rm(list = ls())
gc()

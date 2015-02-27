# Prepare model input dataset (tempDataSync)
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript prepare_model_data.R <input masterData rdata> <input covariateData rdata> <input springFallBPs rdata> <output tempDataSync rdata>
# example: $ Rscript prepare_model_data.R ./masterData.RData ./covariateData.RData ./springFallBPs.RData ./tempDataSync.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

library(ggplot2)
library(ggmcmc) # load before dplyr so later plyr doesn't override dplyr
library(dplyr)
library(DataCombine) # for the slide function
#library(nlme)
library(devtools)
# install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(jsonlite)

config <- fromJSON('model_config.json')

# validate = TRUE # get rid of this once model_config.json is ready

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)


######### add wd argument

masterData_file <- args[1]
if (!file.exists(masterData_file)) {
  stop(paste0('Could not find masterData binary file: ', masterData_file))
}
masterData <- readRDS(masterData_file)

covariateData_file <- args[2]
if (!file.exists(covariateData_file)) {
  stop(paste0('Could not find covariateData binary file: ', covariateData_file))
}
covariateData <- readRDS(covariateData_file)

springFallBPs_file <- args[3]
if (!file.exists(springFallBPs_file)) {
  stop(paste0('Could not find springFallBPs binary file: ', springFallBPs_file))
}
springFallBPs <- readRDS(springFallBPs_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}


masterData <- masterData[, c("agency", "date", "AgencyID", "year", "site", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")]

#tempData <- left_join(tempData, covariateData)


#covariateData <- readStreamTempData(timeSeries=TRUE, covariates=TRUE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

springFallBPs$site <- as.character(springFallBPs$site)

# Join with break points
tempDataBP <- left_join(masterData, springFallBPs, by=c('site', 'year'))

# Clip to syncronized season
tempDataSync <- dplyr::filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

rm(masterData) # save some memory
rm(tempDataBP)

# Order by group and date
tempDataSync <- tempDataSync[order(tempDataSync$site,tempDataSync$year,tempDataSync$dOY),]

# For checking the order of tempDataSync
tempDataSync$count <- 1:length(tempDataSync$year)

tempDataSync <- tempDataSync[order(tempDataSync$count),] # just to make sure tempDataSync is ordered for the slide function

# airTemp
tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')

# prcp
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')

tempDataSync <- left_join(tempDataSync, covariateData)

tempDataSync <- tempDataSync[ , c("agency", "date", "AgencyID", "year", "site", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")]

# consider limiting to small-medium watersheds - add to model_config.json
#if(class(filter.area) == "numeric") tempDataSync <- filter(tempDataSync, filter = TotDASqKM <= filter.area)

tempDataSync <- na.omit(tempDataSync) ####### Needed to take out first few days that get NA in the lagged terms. Change this so don't take out NA in stream temperature?

var.names <- c("Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")

tempDataSync <- mutate(tempDataSync, huc = HUC8)

### Separate data for fitting (training) and validation
#If validating:
if (config[['validate']]) {
  validateFrac <- 0.2
  n.fit <- floor(length(unique(tempDataSync$site)) * (1 - validateFrac))
  
  set.seed(2346)
  site.fit <- sample(unique(tempDataSync$site), n.fit, replace = FALSE) # select sites to hold back for testing 
  tempDataSyncValid <- subset(tempDataSync, !site %in% site.fit) # data for validation
  tempDataSync <- subset(tempDataSync, site %in% site.fit)    # data for model fitting (calibration)
  
  tempDataSyncValidS <- stdCovs(x = tempDataSyncValid, y = tempDataSync, var.names = var.names)
  
  #####################
  means <- mean(tempDataSync[, var.names[[1]]], na.rm = T)
  
  #############
  
  tempDataSyncValidS <- indexDeployments(tempDataSyncValidS, regional = TRUE)
  firstObsRowsValid <- createFirstRows(tempDataSyncValidS)
  evalRowsValid <-createEvalRows(tempDataSyncValidS)
  
} else {
  tempDataSyncValid <- NULL
}

# Standardize for Analysis

tempDataSyncS <- stdFitCovs(x = tempDataSync, var.names = var.names)

tempDataSyncS <- addInteractions(tempDataSyncS)

tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
firstObsRows <- createFirstRows(tempDataSyncS)
evalRows <- createEvalRows(tempDataSyncS)

if (config[['validate']]) {
  
  tempDataSyncValidS <- addInteractions(tempDataSyncValidS)
  
  save(tempDataSync, tempDataSyncS, tempDataSyncValid, tempDataSyncValidS, firstObsRows, evalRows, firstObsRowsValid, evalRowsValid, file = output_file)
} else {
  save(tempDataSync, tempDataSyncS, firstObsRows, evalRows, file = output_file)
}


# Calculate Derived Metrics 
# requires coef.RData, tempDataSync.RData (or saved means and standard deviations for back transformations), covariate_list.RData, springFallBreakpoints.RData, and daymetFullRecordObservedMASites.RData (daymet records for all catchments where we want to do predictions and derived catchments - i.e. all catchments in the region)
#
# returns a csv table of derived metrics for all catchments (could save as RData)
#
# usage: $ Rscript derive_metrics.R <input tempDataSync rdata> <input covariate_list rdata> <input coef rdata> <output rmse_table rdata>
# example: $ Rscript validate_model.R ./tempDataSync.RData ./covariate_list.RData ./coef.RData ./rmse_table.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

tempDataSync_file <- args[1]
if (!file.exists(tempDataSync_file)) {
  stop(paste0('Could not find tempDataSync binary file: ', tempDataSync_file))
}
load(tempDataSync_file)

cov_file <- args[2]
if (!file.exists(cov_file)) {
  stop(paste0('Could not find covariate binary file: ', cov_file))
}
cov.list <- readRDS(cov_file)

coef_file <- args[3]
if (!file.exists(coef_file)) {
  stop(paste0('Could not find covariate binary file: ', coef_file))
}
coef.list <- readRDS(coef_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

# ----

library(ggplot2)
library(reshape2)
library(dplyr)
library(DataCombine) # for the slide function
library(devtools)
install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)


#load((paste0(dataOutDir, 'modSummary.RData')))
load(paste0(dataLocalDir, 'coef.RData'))
load(paste0(dataLocalDir, 'daymetFullRecordObservedMASites.RData'))
load(paste0(dataLocalDir, 'tempDataSync.RData'))
load(paste0(dataLocalDir, 'covariate_list.RData'))
load(paste0(dataLocalDir, 'springFallBreakpoints.RData'))


#Northeast
CTDEP  <- F
MAFW   <- T
MAUSGS <- T
MADEP  <- T 
NHFG   <- F
NHDES  <- F
USFS   <- F
VTFWS  <- F
MEDMR  <- F

#Montana
MTUSGSYellowstone <- F
MTUSGSGlacier <- F

sourceChoice <- list( CTDEP,   MAFW,   MAUSGS, MADEP,   NHFG,   NHDES,   MEDMR,   USFS,   VTFWS,    MTUSGSYellowstone,   MTUSGSGlacier)
sourceNames  <- c   ('CTDEP', 'MAFW', 'MAUSGS', 'MADEP', 'NHFG', 'NHDES', 'MEDMR', 'USFS', 'VTFWS',  'MTUSGSYellowstone', 'MTUSGSGlacier')

dataSource <- sourceNames[sourceChoice == T]

fields <- c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")


var.names <- c("Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")

covariateData <- readStreamTempData(timeSeries=FALSE, covariates=TRUE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

observedData <- readStreamTempData(timeSeries=TRUE, covariates=FALSE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

springFallBPs$site <- as.character(springFallBPs$site)
########## How to add BP for years without data and clip data to the sync period ??? #######
# Join with break points
# covariateDataBP <- left_join(covariateData, springFallBPs, by=c('site', 'year'))
# rm(covariateData)

# temp hack
observedData$site <- as.character(observedData$site)
tempData <- left_join(climateData, select(covariateData, -Latitude, -Longitude), by=c('site'))
tempData <- left_join(tempData, dplyr::select(.data = observedData, agency, date, AgencyID, site, temp), by = c("site", "date"))
tempDataBP <- left_join(tempData, springFallBPs, by=c('site', 'year'))

# Clip to syncronized season
# tempFullSync <- filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

# temp hack - eventually need to adjust Kyle's code to substitute huc or other mean breakpoint in when NA
fullDataSync <- tempDataBP %>%
  filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP)) %>%
  filter(dOY >= mean(finalSpringBP, na.rm = T) & dOY <= mean(finalFallBP, na.rm = T))

rm(climateData) # save some memory
##################

rm(covariateData) # save some memory

# Order by group and date
fullDataSync <- fullDataSync[order(fullDataSync$site,fullDataSync$year,fullDataSync$dOY),]

# For checking the order of fullDataSync
fullDataSync$count <- 1:length(fullDataSync$year)

fullDataSync <- fullDataSync[order(fullDataSync$count),] # just to make sure fullDataSync is ordered for the slide function

# airTemp
fullDataSync <- slide(fullDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
fullDataSync <- slide(fullDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')

# prcp
fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')

fullDataSync <- fullDataSync[ , c("agency", "date", "AgencyID", "year", "site", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")] #  


#if(class(filter.area) == "numeric") fullDataSync <- filter(fullDataSync, filter = TotDASqKM <= filter.area)

#fullDataSync <- na.omit(fullDataSync) ####### Needed to take out first few days that get NA in the lagged terms. Change this so don't take out NA in stream temperature?


# Standardize for Analysis

fullDataSyncS <- stdCovs(x = fullDataSync, y = tempDataSync, var.names = var.names)

fullDataSyncS <- addInteractions(fullDataSyncS)

fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)
#firstObsRowsFull <- createFirstRows(fullDataSyncS)
#evalRowsFull <- createEvalRows(fullDataSyncS)


# save(fullDataSync, fullDataSyncS, file = paste0(dataLocalDir, "fullDataSync.RData"))

#load(paste0(dataOutDir, "tempDataSync-daymet.Rdata"))

fullDataSyncS <- mutate(fullDataSyncS, huc = HUC8)
fullDataSyncS <- predictTemp(data = fullDataSyncS, coef.list = coef.list, cov.list = cov.list)

fullDataSync <- left_join(fullDataSync, select(fullDataSyncS, site, date, tempPredicted), by = c("site", "date"))

test.pred <- filter(fullDataSyncS, !is.na(temp))

rmse(na.omit(test.pred$temp - test.pred$tempPredicted))
ggplot(test.pred, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1, 1), colour = "blue")

# plot observed and predicted vs day of the year for all sites in all years

plotPredict(tempDataSync, fullDataSync, siteList = c("MADEP_W0989_T1", "MAUSGS_WEST_BROOK"), year = "ALL", display = T)

plotPredict(tempDataSync, fullDataSync, siteList = c("MAUSGS_WEST_BROOK"), year = c(1995, 2009:2010), display = T)

# use plotPredict function
plotPredict(observed = tempDataSync, predicted = fullDataSync, siteList = "ALL", yearList = "ALL", dir = paste0(dataLocalDir,'/', 'plots/fullRecord/'))

###Derived metrics
derived.site.metrics <- deriveMetrics(fullDataSync)

summary(derived.site.metrics)

#derived.site.metrics.clean <- na.omit(derived.site.metrics)
write.table(derived.site.metrics, file = 'reports/MADEP/derived_site_metrics.csv', sep = ',', row.names = F)

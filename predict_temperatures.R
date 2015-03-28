# Predicts daily stream temperatures and summarizes the derived metrics

# usage: RScript test_1.R <working directory> <inputs.json> <outputs.json>

# jsonlite is much better than rjson
# library(rjson)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(DataCombine) # for the slide function
library(RPostgreSQL)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

args <- commandArgs(TRUE)
# cmdargs <- fromJSON(args[1])
print(args)

if(file.exists("/conte")) {
  # note: dir is already a function in R, better not to overwrite it, use wd instead
  # also don't need fromJSON since the working directory is just a command line argument
  # dir <- fromJSON(args[1])
  # setwd(dir$wd)
  wd <- args[1]
  setwd(wd)
  
  baseDir <- getwd()
  
  dataInDir <- paste0(baseDir, '/dataIn/')
  dataOutDir <- paste0(baseDir, '/dataOut/')
  dataLocalDir <- paste0(baseDir, '/localData/')
  graphsDir <- paste0(baseDir, '/graphs/')
  
  #source(paste0(baseDir, 'code/functions/temperatureModelingFunctions.R'))
  #source(paste0(baseDir, 'code/functions/dataIndexingFunctions.R'))
  
  #load((paste0(dataOutDir, 'modSummary.RData')))
coef.list <- readRDS(paste0(dataLocalDir, 'coef.RData'))
  load(paste0(dataLocalDir, 'tempDataSync.RData'))
  #load(paste0(dataLocalDir, 'covariate_list.RData'))
  #load(paste0(dataLocalDir, 'springFallBreakpoints.RData'))

cov.list <- readRDS(paste0(dataLocalDir, 'covariate-list.RData'))
springFallBPs <- readRDS(paste0(dataLocalDir, 'springFallBPs.RData'))
  
  # example: $ Rscript retreive_db.R ./temperatureData.RData ./covariateData.RData ./climateData.RData
  
  input_file <- args[2]
  if (!file.exists(input_file)) {
    stop(paste0('Could not find coef binary file: ', input_file))
  }
  coef.list <- readRDS(input_file)
  
  input_file <- args[3]
  if (!file.exists(input_file)) {
    stop(paste0('Could not find tempDataSync binary file: ', input_file))
  }
  load(input_file) # has to be loaded instead of readRDS because contains multiple objects
  
  input_file <- args[4]
  if (!file.exists(input_file)) {
    stop(paste0('Could not find covariate_list binary file: ', input_file))
  }
  cov.list <- readRDS(input_file)
  
  input_file <- args[5]
  if (!file.exists(input_file)) {
    stop(paste0('Could not find springFallBreakpoints binary file: ', input_file))
  }
  springFallBPs <- readRDS(input_file)
  
  # connect to database source
  db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
} else {
  
  db <- src_postgres(dbname='conte_dev', host='ecosheds.org', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
}

#fields <- c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")

#var.names <- c("Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")

# Get list of unique catchments with daymet data in our database
drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname="conte_dev", host="127.0.0.1", user="conte", password="conte")
con <- dbConnect(drv, dbname="conte_dev", host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
qry <- "SELECT DISTINCT featureid FROM daymet;" # add join with drainage area and filter to those < 400
result <- dbSendQuery(con, qry)
catchments <- fetch(result, n=-1)
catchments <- as.character(catchments$featureid)

# get daymet data for a subset of catchments

catchmentid = catchments # until make into a function

#retreiveDaymet <- function(catchmentid) { # make into a function
n.catches <- length(catchmentid)
#if(n.catches == 1) {
#  tbl_daymet <- tbl(db, 'daymet') %>%
#    dplyr::filter(featureid == catchmentid) %>%
#    dplyr::mutate(airTemp = (tmax + tmin)/2) %>%
#} else {
#  tbl_daymet <- tbl(db, 'daymet') %>%
#    dplyr::filter(featureid %in% catchmentid) %>%
#    dplyr::mutate(airTemp = (tmax + tmin)/2) %>%
#}

# Get HUC8
# pass the db$con from dplyr as the connection to RPostgreSQL::dbSendQuery
rs <- dbSendQuery(con, "SELECT c.featureid as featureid, w.huc8 as huc8
                  FROM catchments c
                  JOIN wbdhu8 w
                  ON ST_Contains(w.geom, ST_Centroid(c.geom));")

# fetch results
featureid_huc8 <- fetch(rs, n=-1)

# Get lat and lon
rs <- dbSendQuery(con, "SELECT featureid, 
                  ST_Y(ST_Centroid(geom)) as latitude, 
                  ST_X(ST_Centroid(geom)) as longitude 
                  FROM catchments;")
featureid_lat_lon <- fetch(rs, n=-1) 

featureid_site <- tempDataSyncS %>%
  dplyr::select(featureid, site) %>%
  dplyr::distinct() %>%
  dplyr::mutate(site = as.character(site))

# temporary save entire environment so don't have to pull from dataframe for testing
save.image(file.path(getwd(), "localData/db_pull_for_predictions.RData"))


# size of chunks
chunk.size <- 100
n.loops <- ceiling(n.catches / chunk.size)
j = 0
for(i in 1600:n.loops) {
  j <- j + 1
  k <- j*chunk.size
  if(k <= n.catches) {
    catches <- catchmentid[(1+(j-1)*chunk.size):k]
  } else {
    catches <- catchmentid[(1+(j-1)*chunk.size):n.catches]
  }
  # connect to database source
  #db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user='conte', password='conte')
  #db <- src_postgres(dbname='conte_dev', host='felek.cns.umass.edu', port='5432', user='conte', password='conte')
  db <- src_postgres(dbname='conte_dev', host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
  
  ######### Temporary to use with old model runs ################
  # create a query that lists all locations and creates 'site' column
  qry_locations <- tbl(db, 'locations') %>%
    dplyr::rename(location_id=id, location_name=name, featureid=catchment_id) %>%
    dplyr::select(location_id, location_name, latitude, longitude, featureid, agency_id) %>%
    left_join(tbl(db, 'agencies') %>% 
                dplyr::rename(agency_name=name, agency_id=id) %>% 
                dplyr::select(agency_name, agency_id)) %>%
    dplyr::select(location_id, location_name, featureid, latitude, longitude, agency_name) %>%
    dplyr::filter(agency_name != "TEST") #%>%
  #mutate(site = #concat(agency_name, '_', location_name))
  
  #head(qry_locations)
  
  # check that all sites are unique
  stopifnot(sum(duplicated(collect(qry_locations)$site))==0)
  
  
  # left join qry_locations to daymet to get daymet timeseries for each unique site
  qry_daymet <- tbl(db, 'daymet') %>%
    left_join(select(qry_locations, featureid), featureid, by='featureid') %>%
    dplyr::filter(featureid %in% catches) %>%
    dplyr::mutate(airTemp = (tmax + tmin)/2) #%>%
  #left_join(select(covariateData, -Latitude, -Longitude), by=c('site'))
  
  climateData <- collect(qry_daymet)
  
  # covariates
  #catches <- catchmentid[sample(1:length(catchmentid), 10000)]
  qry_covariates <- tbl(db, 'covariates') %>%
    dplyr::filter(featureid %in% catches)
  
  df_covariates_long <- dplyr::collect(qry_covariates)
  
  df_covariates_wide <- tidyr::spread(df_covariates_long, variable, value)
  
  # which zone? upstream or downstream? - maybe elevation should be downstream all others upstream
  df_covariates_upstream <- dplyr::filter(df_covariates_wide, zone=="upstream")
  
  # check for NA
  # summary(df_covariates_upstream)
  
  
  #masterdata <- left_join(tbl_daymet, df_covariates_upstream, by=c('featureid', 'site'))
  
  mean.spring.bp <- mean(dplyr::filter(springFallBPs, finalSpringBP != "Inf")$finalSpringBP, na.rm = T)
  mean.fall.bp <- mean(dplyr::filter(springFallBPs, finalFallBP != "Inf")$finalFallBP, na.rm = T)
  
  foo <- springFallBPs %>%
    dplyr::mutate(site = as.character(site),
                  featureid = as.integer(site),
                  finalSpringBP = ifelse(finalSpringBP == "Inf" | is.na(finalSpringBP), mean.spring.bp, finalSpringBP),
                  finalFallBP = ifelse(finalFallBP == "Inf" | is.na(finalFallBP), mean.fall.bp, finalFallBP))
  
  ########## How to add BP for years without data and clip data to the sync period ??? #######
  # Join with break points
  # covariateDataBP <- left_join(covariateData, springFallBPs, by=c('site', 'year'))
  # rm(covariateData)
  
  
  fullDataSync <- climateData %>%
    left_join(df_covariates_upstream, by=c('featureid')) %>%
    left_join(dplyr::select(tempDataSync, date, featureid, site, temp), by = c('date', 'featureid')) %>%
    left_join(featureid_huc8, by = c('featureid')) %>%
    left_join(featureid_lat_lon, by = c('featureid')) %>%
    dplyr::mutate(year = as.numeric(format(date, "%Y"))) %>%
    left_join(dplyr::select(foo, -site), by = c('featureid', 'year')) %>%
    dplyr::mutate(dOY = yday(date)) %>%
    dplyr::mutate(huc = huc8) %>%
    dplyr::filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>%
    dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp) %>%
    dplyr::filter(AreaSqKM < 400)
  
  # rm(climateData) removing might slow down because subsequent iterations already have the right size
  
  # make match the old variable names
  # fullDataSync <- fullDataSync %>%
  #    rename(Forest = forest,
  #          ReachElevationM = elev_nalcc,
  #           SurficialCoarseC = surfcoarse,
  #          TotDASqKM = AreaSqKM,
  #         Latitude = latitude,
  #        Longitude = longitude,
  #       CONUSWetland = fwswetlands) %>%
  #mutate(ImpoundmentsAllSqKM = allonnet*TotDASqKM)
  
  # Order by group and date
  fullDataSync <- fullDataSync[order(fullDataSync$featureid, fullDataSync$year, fullDataSync$dOY),]
  
  # For checking the order of fullDataSync
  fullDataSync$count <- 1:length(fullDataSync$year)
  
  fullDataSync <- fullDataSync[order(fullDataSync$count),] # just to make sure fullDataSync is ordered for the slide function
  
  # airTemp
  #fullDataSync <- slide(fullDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
  #fullDataSync <- slide(fullDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')
  
  # prcp
  #fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
  #fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
  #fullDataSync <- slide(fullDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')
  
  # moving means instead of lagged terms in the future
  fullDataSync = fullDataSync %>%
    group_by(featureid, year) %>%
    arrange(featureid, year, dOY) %>%
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
  #fullDataSync <- fullDataSync[ , c("agency", "date", "AgencyID", "year", "site", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")] #  
  
  
  #if(class(filter.area) == "numeric") fullDataSync <- filter(fullDataSync, filter = TotDASqKM <= filter.area)
  
  #fullDataSync <- na.omit(fullDataSync) ####### Needed to take out first few days that get NA in the lagged terms. Change this so don't take out NA in stream temperature?
  
  
  # Standardize for Analysis
  #var.names <- var.names[var.names %!in% c("Developed", "Herbacious", "Agriculture", "HydrologicGroupAB", "CONUSWetland", "ReachSlopePCNT")]
  
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
  
  fullDataSync <- fullDataSync %>%
    mutate(HUC8 = as.character(huc8),
           huc8 = as.character(huc8),
           site = as.numeric(as.factor(featureid))) #Developed = NA, Herbacious = NA, Agriculture = NA, HydrologicGroupAB=NA, ReachSlopePCNT=NA, 
  
  fullDataSyncS <- stdCovs(x = fullDataSync, y = tempDataSync, var.names = var.names)
  
  fullDataSyncS <- addInteractions(fullDataSyncS)
  
  fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)
  #firstObsRowsFull <- createFirstRows(fullDataSyncS)
  #evalRowsFull <- createEvalRows(fullDataSyncS
  
  #save(fullDataSync, fullDataSyncS, file = paste0(dataLocalDir, "fullDataSync.RData"))
  
  
  #load(paste0(dataOutDir, "tempDataSync-daymet.Rdata"))
  
  #fullDataSyncS <- mutate(fullDataSyncS, huc = HUC8)
  fullDataSyncS <- predictTemp(data = fullDataSyncS, coef.list = coef.list, cov.list = cov.list)
  
  fullDataSync <- left_join(fullDataSync, select(fullDataSyncS, featureid, date, tempPredicted), by = c("featureid", "date"))
  
  
  # Remove any unnecesary columns so can then remove any rows with NA
  # This list should be made in the analysis script, saved and loaded here
  # maybe not necessary if all the derived metrics are based on predicted temperature and airTemp
  #fullDataSync <- fullDataSync %>%
  # select(featureid
  #       , site
  #      , date
  #     , year
  #    , tmax
  #   , tmin
  #  , prcp
  #   , dayl
  #  , vp
  # , srad
  # , swe
  # , airTemp)
  
  #fullDataSync <- fullDataSync %>%
  # filter(!is.na(tempPredicted)) %>%
  #  filter(!is.na(dOY)) %>%
  #  filter(!is.na(airTemp))
  # add state and anything else needed for metrics in the future
  
  #test.pred <- filter(fullDataSyncS, !is.na(temp))
  
  #rmse(na.omit(test.pred$temp - test.pred$tempPredicted))
  #ggplot(test.pred, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1, 1), colour = "blue")
  
  
  # plot observed and predicted vs day of the year for all sites in all years
  
  #plotPredict(tempDataSync, fullDataSync, siteList = c("MADEP_W0989_T1", "MAUSGS_WEST_BROOK"), year = "ALL", display = T)
  
  #plotPredict(tempDataSync, fullDataSync, siteList = c("MAUSGS_WEST_BROOK"), year = c(1995, 2009:2010), display = T)
  
  # use plotPredict function
  #plotPredict(observed = tempDataSync, predicted = fullDataSync, siteList = "ALL", yearList = "ALL", dir = paste0(dataLocalDir,'/', 'plots/fullRecord/'))
  
  #ggplot(filter(fullDataSyncS, featureid == catches[1]), aes(dOY, tempPredicted)) + geom_point() + facet_wrap(~year)
  
  ###Derived metrics
  #dim(fullDataSync)
  #summary(fullDataSync)
  #unique(fullDataSync$huc)
  #length(unique(fullDataSync$featureid))
  #str(fullDataSyncS$tempPredicted)
  
  mean.pred <- mean(fullDataSync$tempPredicted, na.rm = T)
  
  str(mean.pred)
  summary(fullDataSync$tempPredicted)
  
  if(mean.pred == "NaN") {
    print(paste0(i, " of ", n.loops, " loops has no predicted temperatures"))
    gc()
  } else {
    derived.site.metrics <- deriveMetrics(fullDataSync)
    
    #metrics <- data.frame(matrix(NA, 1, dim(derived)))
    if(exists("metrics") == FALSE) {
      metrics <- derived.site.metrics
      #print("no")
    } else {
      metrics <- rbind(metrics, derived.site.metrics)
    }
    
    #summary(metrics)
    
    print(paste0(i, " of ", n.loops, " loops"))
    write.table(metrics, file = 'derived_site_metrics_loop.csv', sep = ',', row.names = F)
    
    gc()
  }
  
}
#derived.site.metrics.clean <- na.omit(derived.site.metrics)

# add flags once have all catchment RMSE
# Flag based on RMSE > 95%
# add an ifelse for whether data is present or not #######################
#derivedfeatureidMetrics <- mutate(derivedfeatureidMetrics, flag = ifelse(meanRMSE > quantile(derivedfeatureidMetrics$meanRMSE, probs = c(0.95), na.rm=TRUE), "Flag", ""))

metrics.lat.lon <- left_join(featureid_lat_lon, derived.site.metrics, by = c('featureid')) # reverse this join or full join so get NA for all missing catchments? - doesn't seem to be working correctly yet - check again

saveRDS(metrics.lat.lon, file = 'derived_site_metrics.RData')
write.table(metrics, file = 'derived_site_metrics.csv', sep = ',', row.names = F)

#}



# Predicts daily stream temperatures and summarizes the derived metrics

# usage: RScript test_1.R <working directory> <inputs.json> <outputs.json>

# jsonlite is much better than rjson
# library(rjson)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(DataCombine) # for the slide function
library(RPostgreSQL)
library(devtools)
install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

args <- commandArgs(TRUE)
# cmdargs <- fromJSON(args[1])
print(args)

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
load(paste0(dataLocalDir, 'coef.RData'))
load(paste0(dataLocalDir, 'tempDataSync.RData'))
load(paste0(dataLocalDir, 'covariate_list.RData'))
load(paste0(dataLocalDir, 'springFallBreakpoints.RData'))

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

input_file <- args[2]
if (!file.exists(input_file)) {
  stop(paste0('Could not find springFallBreakpoints binary file: ', input_file))
}
springFallBPs <- readRDS(input_file)

fields <- c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")

var.names <- c("Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")

# Get list of unique catchments with daymet data in our database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="conte_dev", host="127.0.0.1", user="conte", password="conte")
# con <- dbConnect(drv, dbname="conte_dev", host="felek.cns.umass.edu", user="conte", password="conte")
qry <- "SELECT DISTINCT featureid FROM daymet;"
result <- dbSendQuery(con, qry)
catchments <- fetch(result, n=-1)
catchments <- as.character(catchments$featureid)

# get daymet data for a subset of catchments

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
rs <- dbSendQuery(db$con, "SELECT c.featureid as featureid, w.huc8 as huc8
FROM catchments c
JOIN wbdhu8 w
ON ST_Contains(w.geom, ST_Centroid(c.geom));")

# fetch results
featureid_huc8 <- fetch(rs, n=-1)


  # size of chunks
  chunk.size <- 10
  n.loops <- ceiling(n.catches / chunk.size)
  j = 0
  for(i in 1:n.loops) {
    j <- j + 1
    k <- j*chunk.size
    if(k <= n.catches) {
      catches <- catchmentid[(1+(j-1)*chunk.size):k]
    } else {
      catches <- catchmentid[(1+(j-1)*chunk.size):n.catches]
    }
  # connect to database source
  #db <- src_postgres(dbname='conte_dev', host='127.0.0.1', port='5432', user='conte', password='conte')
   db <- src_postgres(dbname='conte_dev', host='felek.cns.umass.edu', port='5432', user='conte', password='conte')
  
  ######### Temporary to use with old model runs ################
  # create a query that lists all locations and creates 'site' column
  qry_locations <- tbl(db, 'locations') %>%
    rename(location_id=id, location_name=name, featureid=catchment_id) %>%
    select(location_id, location_name, latitude, longitude, featureid, agency_id) %>%
    left_join(tbl(db, 'agencies') %>% rename(agency_name=name, agency_id=id) %>% select(agency_name, agency_id)) %>%
    select(location_id, location_name, featureid, latitude, longitude, agency_name) %>%
    mutate(site=concat(agency_name, '_', location_name))
  
  head(qry_locations)
  
  # check that all sites are unique
  stopifnot(sum(duplicated(collect(qry_locations)$site))==0)
  
  
  # left join qry_locations to daymet to get daymet timeseries for each unique site
  qry_daymet <- tbl(db, 'daymet') %>%
    left_join(select(qry_locations, site, featureid), by='featureid') %>%
    filter(featureid %in% catches) %>%
    mutate(airTemp = (tmax + tmin)/2) %>%
    left_join(climateData, select(covariateData, -Latitude, -Longitude), by=c('site'))
  
climateData <- collect(qry_daymet)

# covariates
#catches <- catchmentid[sample(1:length(catchmentid), 10000)]
qry_covariates <- tbl(db, 'covariates') %>%
  filter(featureid %in% catches)

df_covariates_long <- collect(qry_covariates)

df_covariates_wide <- tidyr::spread(df_covariates_long, variable, value)

# which zone? upstream or downstream?
df_covariates_upstream <- filter(df_covariates_wide, zone=="upstream")

# check for NA
summary(df_covariates_upstream)


#masterdata <- left_join(tbl_daymet, df_covariates_upstream, by=c('featureid', 'site'))
  
springFallBPs$site <- as.character(springFallBPs$site)

########## How to add BP for years without data and clip data to the sync period ??? #######
# Join with break points
# covariateDataBP <- left_join(covariateData, springFallBPs, by=c('site', 'year'))
# rm(covariateData)
mean.spring.bp <- mean(springFallBPs$finalSpringBP, na.rm = T)
mean.fall.bp <- mean(springFallBPs$finalFallBP, na.rm = T)


fullDataSync <- climateData %>%
  left_join(df_covariates_upstream, by=c('featureid')) %>%
  left_join(select(tempDataSync, date, site, temp), by = c('date', 'site')) %>%
  left_join(featureid_huc8, by = c('featureid')) %>%
  mutate(year = as.POSIXlt(date)$year) %>%
  left_join(springFallBPs, by = c('site', 'year')) %>%
  mutate(dOY = strptime(date, "%Y-%m-%d")$yday+1) %>%
  rename(huc = huc8) %>%
  filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP)) %>%
  filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp)

# make match the old variable names
fullDataSync <- fullDataSync %>%
  rename(Forest = forest,
         ReachElevationM = elev_nalcc,
         SuficialCoarseC = surfcoarse,
         TotDASqKM = AreaSqKM) %>%
  mutate(ImpoundmentsAllSqKM = allonnet*TotDASqKM)

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

#fullDataSync <- fullDataSync[ , c("agency", "date", "AgencyID", "year", "site", "date", "finalSpringBP", "finalFallBP", "FEATUREID", "HUC4", "HUC8", "HUC12", "temp", "Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")] #  


#if(class(filter.area) == "numeric") fullDataSync <- filter(fullDataSync, filter = TotDASqKM <= filter.area)

#fullDataSync <- na.omit(fullDataSync) ####### Needed to take out first few days that get NA in the lagged terms. Change this so don't take out NA in stream temperature?


# Standardize for Analysis
var.names <- var.names[var.names %!in% c("Developed", "Herbacious", "Agriculture", "HydrologicGroupAB", "CONUSWetland", "ReachSlopePCNT")]
  
fullDataSyncS <- stdCovs(x = fullDataSync, y = tempDataSync, var.names = var.names)

fullDataSyncS <- addInteractions(fullDataSyncS)

fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)
#firstObsRowsFull <- createFirstRows(fullDataSyncS)
#evalRowsFull <- createEvalRows(fullDataSyncS)


#save(fullDataSync, fullDataSyncS, file = paste0(dataLocalDir, "fullDataSync.RData"))


#load(paste0(dataOutDir, "tempDataSync-daymet.Rdata"))

#fullDataSyncS <- mutate(fullDataSyncS, huc = HUC8)
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

}

# example of resistance
fullDataSync <- mutate(fullDataSync, year = as.numeric(year))
WB.2011.summer <- fullDataSync[which(fullDataSync$site == "MAUSGS_WEST_BROOK" & as.numeric(fullDataSync$year) == 2011 & fullDataSync$dOY >=145 & fullDataSync$dOY <= 275), ]
sum(WB.2011.summer$airTemp - WB.2011.summer$tempPredicted)

ggplot(fullDataSync[which(fullDataSync$site == "MAUSGS_WEST_BROOK" & fullDataSync$year == 2011), ], aes(dOY, tempPredicted)) + 
  geom_point(size=2, colour = "red") + geom_line(colour = 'red') +
  geom_point(data=fullDataSync[which(fullDataSync$site == "MAUSGS_WEST_BROOK" & fullDataSync$year == 2011), ], aes(dOY, airTemp), colour = "black", size=2) + 
  geom_line(data=fullDataSync[which(fullDataSync$site == "MAUSGS_WEST_BROOK" & fullDataSync$year == 2011), ], aes(dOY, airTemp), colour = "black") + 
  geom_ribbon(data = fullDataSync[which(fullDataSync$site == "MAUSGS_WEST_BROOK" & fullDataSync$year == 2011 & fullDataSync$dOY >=145 & fullDataSync$dOY <= 275), ], aes(x=dOY, ymin=tempPredicted, ymax=airTemp), fill="dark grey", alpha=.5) +
  xlab("Day of the year") +
  ylab("Temperature (C)") #+ theme_classic()

ggplot(fullDataSync[which(fullDataSync$site == "WB OBEAR" & fullDataSync$year == 2010), ], aes(dOY.real, tempPredicted)) + 
  geom_point(size=2, colour = "black") + geom_line(colour = 'black') +
  geom_abline(intercept = 18, slope=0, colour='red') +
  geom_point(data = fullDataSync[which(fullDataSync$site == "WB OBEAR" & fullDataSync$year == 2010 & fullDataSync$tempPredicted >= 18), ], aes(dOY.real, tempPredicted), colour='red') +
  xlab("Day of the year") +
  ylab("Stream temperature (C)") #+ theme_classic()

# Reset ggplot2 theme default to gray
theme_set(theme_gray())



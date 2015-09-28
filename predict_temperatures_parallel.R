# Predicts daily stream temperatures and summarizes the derived metrics

# usage: RScript test_1.R <working directory> <inputs.json> <outputs.json>

# jsonlite is much better than rjson
# library(rjson)
library(jsonlite)
library(stringr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(zoo) # for rollapply function
library(lubridate)
#library(DataCombine) # for the slide function
library(RPostgreSQL)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

data_dir <- "localData_2015-08-24" 

args <- commandArgs(TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/coef.RData"), paste0(data_dir, "/tempDataSync.RData"), paste0(data_dir, "/covariate_list.RData"),  paste0(data_dir, "/springFallBPs.RData")) # 
}

input_file <- args[1]
if (!file.exists(input_file)) {
  stop(paste0('Could not find coef binary file: ', input_file))
}
coef.list <- readRDS(input_file)

input_file <- args[2]
if (!file.exists(input_file)) {
  stop(paste0('Could not find tempDataSync binary file: ', input_file))
}
load(input_file) # has to be loaded instead of readRDS because contains multiple objects

input_file <- args[3]
if (!file.exists(input_file)) {
  stop(paste0('Could not find covariate_list binary file: ', input_file))
}
cov.list <- readRDS(input_file)

input_file <- args[4]
if (!file.exists(input_file)) {
  stop(paste0('Could not find springFallBreakpoints binary file: ', input_file))
}
springFallBPs <- readRDS(input_file)


########## connect to database ##########
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="sheds", host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

########## Get List of featureid to predict ##########
# Get list of unique catchments with daymet data
qry <- "SELECT DISTINCT featureid FROM daymet;" # add join with drainage area and filter to those < 400
result <- dbSendQuery(con, qry)
catchments <- fetch(result, n=-1)
catchments <- as.character(catchments$featureid)

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

########## Get HUC Info ##########
qry_huc <- paste0("SELECT featureid, huc12 FROM catchment_huc12;")

rs <- dbSendQuery(con, statement = qry_huc)
df_huc <- fetch(rs, n=-1)

df_huc <- df_huc %>%
  dplyr::mutate(HUC4=str_sub(huc12, 1, 4),
                HUC8=str_sub(huc12, 1, 8),
                HUC10=str_sub(huc12, 1, 10),
                huc = as.character(huc12)) %>%
  dplyr::rename(HUC12 = huc12)

# Get HUC8 - temporary for legacy code
featureid_huc8 <- df_huc %>%
  dplyr::select(featureid, HUC8, HUC12, huc)


########## Get lat and lon ##########
rs <- dbSendQuery(con, "SELECT featureid, 
                  ST_Y(ST_Centroid(geom)) as latitude, 
                  ST_X(ST_Centroid(geom)) as longitude 
                  FROM catchments;")
featureid_lat_lon <- fetch(rs, n=-1) 


########## Get Landscape Covariates ########## 
# get covariates outside loop then subset within loop rather than pull from database each iteration

param_list <- c("forest", 
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

cov_list_string <- paste(shQuote(param_list), collapse = ", ")

qry_covariates <- paste0("SELECT * FROM covariates WHERE zone='upstream' AND variable IN (", cov_list_string, ") ;") # could add AreaSqKM <400
rs <- dbSendQuery(con, qry_covariates)
df_covariates_long <- fetch(rs, n=-1)

# transform from long to wide format
df_covariates_upstream <- tidyr::spread(df_covariates_long, variable, value)


########## featureid-site ########## 
# not sure if this is needed
featureid_site <- tempDataSyncS %>%
  dplyr::select(featureid, site) %>%
  dplyr::distinct() %>%
  dplyr::mutate(site = as.character(site))

########## Temporary save for testing ########## 
# temporary save entire environment so don't have to pull from dataframe for testing
save.image(file.path(getwd(), paste0(data_dir, "/db_pull_for_predictions.RData")))

dbClearResult(res = rs)
dbDisconnect(con)
dbUnloadDriver(drv)
#dbListConnections(drv)

gc()

#------------------loop through all catchments----------- 
# ?could speed up by excluding large drainage areas?

########## Set Up Parallel Processing ##########
library(foreach)
#library(doMC) # doesn't work well with connecting to the database
library(doParallel)
library(RPostgreSQL)

# size of chunks
chunk.size <- 10
n.loops <- ceiling(n.catches / chunk.size)

# set up parallel backend & make database connection available to all workers
nc <- min(c(detectCores()-1, 12)) # use the maximum number of cores minus 1 or up to 15 because max 16 database connections - changed to 12 since Evan also often using osensei on multiple cores
cl <- makeCluster(nc, type = "PSOCK") # try PSOCK instead of more memory efficient "FORK" to prevent hanging at end: http://www.stat.berkeley.edu/scf/paciorek-parallel-2014.pdf
registerDoParallel(cl)

# setup to write out to monitor progress
logFile = paste0(data_dir, "/log_file.txt")
logFile_Finish = paste0(data_dir, "/log_file_finish.txt")
cat("Monitoring progress of prediction loop in parallel", file=logFile, append=FALSE, sep = "\n")
cat("Monitoring the finish of each loop", file=logFile_Finish, append=FALSE, sep = "\n")

########## Run Parallel Loop ########## 
# start loop
derived.site.metrics <- foreach(i = 1:n.loops, 
                   .inorder=FALSE, 
                   .combine = rbind,
                   .packages=c("DBI", 
                               "RPostgreSQL",
                               "reshape2",
                               "dplyr",
                               "tidyr",
                               "zoo",
                               "lubridate",
                               "conteStreamTemperature"),
                   .export = c("indexDeployments") # shouldn't be needed after update package
                   #.export = c("derive_metrics_par")#,
                  # .export=ls(envir=globalenv())
) %dopar% {
  
  #for(i in 1:n.loops) {
  
  ########## Set up database connection ##########
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname='sheds', host='felek.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
  # write start of each iteration
  start.time <- Sys.time()
  cat(paste(start.time, ": Starting iteration", i, " of ", n.loops, "\n"), file = logFile, append = TRUE)
  
  ########## Set group of featureid to calculate for ##########
  k <- i*chunk.size
  if(k <= n.catches) {
    catches <- catchmentid[(1+(i-1)*chunk.size):k]
  } else {
    catches <- catchmentid[(1+(i-1)*chunk.size):n.catches]
  }
  catches_string <- paste(catches, collapse = ', ')
  
  # reconnect to database if lost
     if(isPostgresqlIdCurrent(con) == FALSE) {
       drv <- dbDriver("PostgreSQL")
       con <- dbConnect(drv, dbname='sheds', host='felek.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
     }
  
  ########## pull daymet records ##########
  qry_daymet <- paste0("SELECT featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe, (tmax + tmin) / 2.0 AS airTemp FROM daymet WHERE featureid IN (", catches_string, ") ;")
  rs <- dbSendQuery(con, statement = qry_daymet)
  climateData <- dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  
  ########## Assign synchronized period ##########
  mean.spring.bp <- mean(dplyr::filter(springFallBPs, finalSpringBP != "Inf")$finalSpringBP, na.rm = T)
  mean.fall.bp <- mean(dplyr::filter(springFallBPs, finalFallBP != "Inf")$finalFallBP, na.rm = T)
  
  foo <- springFallBPs %>%
    dplyr::mutate(site = as.character(site),
                  featureid = as.integer(site),
                  finalSpringBP = ifelse(finalSpringBP == "Inf" | is.na(finalSpringBP), mean.spring.bp, finalSpringBP),
                  finalFallBP = ifelse(finalFallBP == "Inf" | is.na(finalFallBP), mean.fall.bp, finalFallBP))
  
  ########## Combine Datat ##########
  fullDataSync <- climateData %>%
    left_join(df_covariates_upstream, by=c('featureid')) %>%
    left_join(dplyr::select(tempDataSync, date, featureid, site, temp), by = c('date', 'featureid')) %>%
    left_join(featureid_huc8, by = c('featureid')) %>%
    left_join(featureid_lat_lon, by = c('featureid')) %>%
    dplyr::mutate(year = as.numeric(format(date, "%Y")),
                  airTemp = (tmax + tmin)/2) %>%
    left_join(dplyr::select(foo, -site), by = c('featureid', 'year')) %>%
    dplyr::mutate(finalSpringBP = ifelse(finalSpringBP == "Inf" | is.na(finalSpringBP), mean.spring.bp, finalSpringBP),
                  finalFallBP = ifelse(finalFallBP == "Inf" | is.na(finalFallBP), mean.fall.bp, finalFallBP)) %>%
    dplyr::mutate(dOY = yday(date)) %>%
    dplyr::filter(AreaSqKM < 200)
   # dplyr::filter(AreaSqKM >= 1 & AreaSqKM < 200 & allonnet < 70) # changed so don't deal with problematically small drainage areas (somre were 0.00006 sq km) - for loop didn't like this!!!!!!!!!
  
  ################### PROBLEM ################
  # if allonnet is very large (maybe > 75% of drainage area) the predictions are probably non-sense 
  ##########################
  
  ################### PROBLEM #################
  # 2-day precip as large as 210 - not sure if this is realistic and if so it might be outside the scope of our predictions
  ##################################
  
  
  rm(climateData)
  gc()
  
  # Order by group and date
  fullDataSync <- fullDataSync[order(fullDataSync$featureid, fullDataSync$year, fullDataSync$dOY),]
  
  # For checking the order of fullDataSync
  #fullDataSync$count <- 1:length(fullDataSync$year)
  
  #fullDataSync <- fullDataSync[order(fullDataSync$count),] # just to make sure fullDataSync is ordered for the slide function
  
  # moving means instead of lagged terms in the future
  fullDataSync <- fullDataSync %>%
    group_by(featureid, year) %>%
    arrange(featureid, year, dOY) %>%
    mutate(impoundArea = AreaSqKM * allonnet,
           airTempLagged1 = lag(airTemp, n = 1, fill = NA),
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
  
  # clip to synchronized period of the year
  #dplyr::mutate(huc = huc8) %>%
  
  ############ PROBLEM ############################
  # not assigning breakpoints properly - mostly NA 
  #############
  
  fullDataSync <- fullDataSync %>%
    dplyr::filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>%
    dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp)
  
  var.names <- c("airTemp", 
                 "temp7p",
                 "prcp", 
                 "prcp2",
                 "prcp7",
                 "prcp30",
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
    mutate(HUC8 = as.character(HUC8),
           huc8 = as.character(HUC8),
           huc = as.character(HUC12),
           site = as.numeric(as.factor(featureid))) 
  
  ################# PROBLEM ###############################
  # HUGE VALUES FOR STANDARDIZED COVARIATES 
  # consider adjusting those for agriculture and herbaceous
  # maybe also for precip
  ###########
  
  fullDataSyncS <- stdCovs(x = fullDataSync, y = df_stds, var.names = var.names)
  
  fullDataSyncS <- addInteractions(fullDataSyncS)
  
  fullDataSyncS <- dplyr::arrange(fullDataSyncS, featureid, date)
  
  fullDataSyncS$site <- as.character(fullDataSyncS$featureid)
  
  if(exists("fullDataSyncS$sitef")) {
    fullDataSyncS <- dplyr::select(fullDataSyncS, -sitef)
  }
  
  fullDataSyncS <- fullDataSyncS %>%
    left_join(rand_ids$df_site) %>%
    left_join(rand_ids$df_huc) %>%
    left_join(rand_ids$df_year)
  
  fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)

  ############# Predictions ##############
  #fullDataSyncS <- predictTemp(data = fullDataSyncS, coef.list = coef.list, cov.list = cov.list, featureid_site = featureid_site)
  
  fixed.ef <- as.numeric(coef.list$B.fixed$mean) # separate out the iteration or do for mean/median
  
  # add specific random effects to the dataframe
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.site, by = "sitef")
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.huc, by = "hucf") # problem with validation data, need to use the mean when huc don't match
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.year, by = "yearf")
  
  
  for (j in 2:length(names(coef.list$B.site))) {
    fullDataSyncS[, names(coef.list$B.site[j])][is.na(fullDataSyncS[, names(coef.list$B.site[j])])] <- colMeans(coef.list$B.site[j])
  }
  for (j in 2:length(names(coef.list$B.huc))) {
    fullDataSyncS[, names(coef.list$B.huc[j])][is.na(fullDataSyncS[, names(coef.list$B.huc[j])])] <- colMeans(coef.list$B.huc[j])
  }
  for (j in 2:length(names(coef.list$B.year))) {
    fullDataSyncS[, names(coef.list$B.year[j])][is.na(fullDataSyncS[, names(coef.list$B.year[j])])] <- colMeans(coef.list$B.year[j])
  }
  
  fullDataSyncS$fixed.ef <- as.vector(fixed.ef %*% t(as.matrix(as.data.frame(unclass(select(ungroup(fullDataSyncS), one_of(cov.list$fixed.ef)))))))
  fullDataSyncS$site.ef <- rowSums(as.matrix(select(fullDataSyncS, one_of(cov.list$site.ef))) * as.matrix(select(fullDataSyncS, starts_with("B.site"))))
  fullDataSyncS$huc.ef <- rowSums(as.matrix(select(fullDataSyncS, one_of(cov.list$huc.ef))) * as.matrix(select(fullDataSyncS, starts_with("B.huc"))))
  fullDataSyncS$year.ef <- rowSums(as.matrix(select(fullDataSyncS, one_of(cov.list$year.ef))) * as.matrix(select(fullDataSyncS, starts_with("B.year"))))
  
  # fullDataSyncS$trend <- rowSums(as.matrix(dplyr::select(fullDataSyncS, one_of(c("fixed.ef", "site.ef", "huc.ef", "year.ef")))))
  # FAILS
  
  #fullDataSyncS <- fullDataSyncS %>%
  # dplyr::mutate(trend = fixed.ef + site.ef + huc.ef + year.ef) # works fine outside foreach
  
  fullDataSyncS$trend <- fullDataSyncS$fixed.ef + fullDataSyncS$site.ef + fullDataSyncS$huc.ef + fullDataSyncS$year.ef # WORKS!!!
  
  # Add B.ar1 to predictions
  #fullDataSyncS <- group_by(fullDataSyncS, sitef)
  fullDataSyncS <- mutate(fullDataSyncS, prev.temp = c(NA, fullDataSyncS$temp[(2:(nrow(fullDataSyncS))) -1]),
                          prev.trend = c(NA, fullDataSyncS$trend[(2:nrow(fullDataSyncS)) - 1]),
                          prev.err = prev.temp - prev.trend,
                          tempPredicted = trend,
                          prev.temp = ifelse(newDeploy == 1, NA, prev.temp),
                          prev.err = ifelse(newDeploy == 1, NA, prev.err))
  #B.ar1.sub <- data.frame(sitef = rand_ids$df_site$sitef)
  B.ar1.sub <- coef.list$B.ar1 %>%
    dplyr::select(sitef, mean) %>%
    dplyr::rename(B.ar1 = mean)
  fullDataSyncS <- left_join(fullDataSyncS, B.ar1.sub, by = c("sitef"))
  fullDataSyncS <- fullDataSyncS %>%
    dplyr::mutate(B.ar1 = ifelse(is.na(B.ar1), mean(B.ar1.sub$B.ar1, na.rm = T), B.ar1)) %>%
    dplyr::arrange(featureid, date)
  
  fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$tempPredicted <- fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$trend + fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$B.ar1 * fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$prev.err

  # for testing and visual inspection
  if(FALSE) {
    ggplot(fullDataSyncS, aes(temp, tempPredicted)) + geom_point()
    ggplot(fullDataSyncS, aes(date, tempPredicted)) + geom_point()
    ggplot(fullDataSyncS, aes(dOY, tempPredicted)) + geom_point() + facet_wrap(~year)
  }
  
  fullDataSync <- left_join(fullDataSync, select(fullDataSyncS, featureid, date, trend, tempPredicted), by = c("featureid", "date"))
  
  # unofficial warning message
  mean.pred <- mean(fullDataSync$tempPredicted, na.rm = T)
  if(mean.pred == "NaN") {
    cat(paste0(i, " of ", n.loops, " loops has no predicted temperatures"))
  } 
  
  ######################## DERIVE METRICS ##################################
#   library(dplyr)
#   library(zoo)
  
  byfeatureid <- group_by(fullDataSync, featureid)
  byfeatureidYear <- group_by(byfeatureid, year, add = TRUE)
  #(maxTempfeatureid <- dplyr::dplyr::summarise(byfeatureid, max(tempPredicted, na.rm = T)))
  
  derivedfeatureidMetrics <- dplyr::select(byfeatureid, featureid) %>%
    distinct
  
  # Mean maximum daily mean temperature by featureid (over years) - this must be calculated before totalObs to work with NA and left join if all fullDataSync missing
  meanMaxTemp <- byfeatureidYear %>%
    dplyr::summarise(maxTempPredicted = max(tempPredicted, na.rm = T)) %>%
    dplyr::summarise(meanMaxTemp = mean(maxTempPredicted))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanMaxTemp, by = "featureid")
  rm(meanMaxTemp)
  
  # total observations (days with fullDataSync) per featureid
  totalObs <- byfeatureidYear %>%
    dplyr::filter(!is.na(temp)) %>%
    dplyr::summarise(Obs = n()) %>%
    dplyr::summarise(totalObs = sum(Obs)) %>%
    dplyr::select(featureid, totalObs)
  derivedfeatureidMetrics <- dplyr::left_join(derivedfeatureidMetrics, totalObs, by = "featureid") %>%
    dplyr::mutate(totalObs = ifelse(is.na(totalObs), 0, as.numeric(totalObs)))
  derivedfeatureidMetrics <- dplyr::select(derivedfeatureidMetrics, featureid, totalObs, meanMaxTemp)
  
  # Maximum max daily mean temperature
  maxMaxTemp <- byfeatureidYear %>%
    dplyr::summarise(maxTemp = max(tempPredicted, na.rm = T)) %>%
    dplyr::summarise(maxMaxTemp = max(maxTemp))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, maxMaxTemp, by = "featureid")
  rm(maxMaxTemp)
  
  # Mean July Summer Temp
  meanJulyTemp <- byfeatureidYear %>%
    dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
    dplyr::filter(month == 7) %>%
    dplyr::summarise(JulyTemp = mean(tempPredicted, na.rm = T)) %>%
    dplyr::summarise(meanJulyTemp = mean(JulyTemp))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanJulyTemp, by = "featureid")
  rm(meanJulyTemp)
  
  # Mean Aug Temp
  meanAugTemp <- byfeatureidYear %>%
    dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
    dplyr::filter(month == 8) %>%
    dplyr::summarise(AugTemp = mean(tempPredicted, na.rm = T)) %>%
    dplyr::summarise(meanAugTemp = mean(AugTemp))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanAugTemp, by = "featureid")
  rm(meanAugTemp)
  
  # Mean Summer Temp
  meanSummerTemp <- byfeatureidYear %>%
    dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
    dplyr::filter(month >= 6 & month <= 8) %>%
    dplyr::summarise(SummerTemp = mean(tempPredicted, na.rm = T)) %>%
    dplyr::summarise(meanSummerTemp = mean(SummerTemp))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanSummerTemp, by = "featureid")
  rm(meanSummerTemp)
  
  # Annual mean 30-day maximum mean daily temperature
  mean30Day <- byfeatureidYear %>%
    arrange(featureid, year, dOY) %>%
    mutate(mm30Day = rollapply(data = tempPredicted, 
                              width = 30, 
                              FUN = mean, 
                              align = "right", 
                              fill = NA, 
                              na.rm = T)) %>%
    dplyr::summarise(max30DayYear = max(mm30Day, na.rm = T)) %>%
    dplyr::summarise(mean30DayMax = mean(max30DayYear, na.rm = T))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, mean30Day, by = "featureid")
  rm(mean30Day)
  
  # Number of days with stream temp > threshold
  derivedfeatureidMetrics <- calcThresholdDays(byfeatureidYear, derivedfeatureidMetrics, 18) %>%
    dplyr::rename(meanDays.18 = meanDays) %>%
    dplyr::mutate(meanDays.18 = as.numeric(meanDays.18)) %>%
  dplyr::mutate(meanDays.18 = ifelse(!is.na(meanMaxTemp) & is.na(meanDays.18), 0, meanDays.18))
  derivedfeatureidMetrics <- calcThresholdDays(byfeatureidYear, derivedfeatureidMetrics, 20) %>%
    dplyr::rename(meanDays.20 = meanDays) %>%
    dplyr::mutate(meanDays.20 = as.numeric(meanDays.20)) %>%
  dplyr::mutate(meanDays.20 = ifelse(!is.na(meanMaxTemp) & is.na(meanDays.20), 0, meanDays.20))
  derivedfeatureidMetrics <- calcThresholdDays(byfeatureidYear, derivedfeatureidMetrics, 22) %>%
    dplyr::rename(meanDays.22 = meanDays) %>%
    dplyr::mutate(meanDays.22 = as.numeric(meanDays.22)) %>%
  dplyr::mutate(meanDays.22 = ifelse(!is.na(meanMaxTemp) & is.na(meanDays.22), 0, meanDays.22))
  #if(class(threshold) == "numeric") derivedfeatureidMetrics <- calcThresholdDays(byfeatureidYear, derivedfeatureidMetrics, threshold)
  
  # CT DEEP Thresholds
  #derivedfeatureidMetrics <- calcYearsCold(byfeatureidYear, derivedfeatureidMetrics, states = c("CT"))
  
  # Number and frequency of years with mean max over threshold
  derivedfeatureidMetrics <- calcYearsMaxTemp(grouped.df = byfeatureidYear, derived.df = derivedfeatureidMetrics, temp.threshold = 18) %>%
    dplyr::mutate(yearsMaxTemp = ifelse(is.na(yearsMaxTemp), 0, as.numeric(yearsMaxTemp))) %>%
    dplyr::rename(yearsMaxTemp.18 = yearsMaxTemp) %>%
    dplyr::mutate(freqMaxTemp.18 = yearsMaxTemp.18 / length(unique(byfeatureidYear$year)))
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "yearsMaxTemp.18"] <- NA
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "freqMaxTemp.18"] <- NA
  derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
    dplyr::mutate(yearsMaxTemp.18 = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.18), 0, yearsMaxTemp.18),
                  yearsMaxTemp.18 = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.18), 0, yearsMaxTemp.18))
  
  derivedfeatureidMetrics <- calcYearsMaxTemp(grouped.df = byfeatureidYear, derived.df = derivedfeatureidMetrics, temp.threshold = 20) %>%
    dplyr::mutate(yearsMaxTemp = ifelse(is.na(yearsMaxTemp), 0, as.numeric(yearsMaxTemp))) %>%
    dplyr::rename(yearsMaxTemp.20 = yearsMaxTemp) %>%
    dplyr::mutate(freqMaxTemp.20 = yearsMaxTemp.20 / length(unique(byfeatureidYear$year)))
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "yearsMaxTemp.20"] <- NA
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "freqMaxTemp.20"] <- NA
  derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
    dplyr::mutate(yearsMaxTemp.20 = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.20), 0, yearsMaxTemp.20),
                  freqMaxTemp.20 = ifelse(!is.na(meanMaxTemp) & is.na(freqMaxTemp.20), 0, freqMaxTemp.20))
  
  derivedfeatureidMetrics <- calcYearsMaxTemp(grouped.df = byfeatureidYear, derived.df = derivedfeatureidMetrics, temp.threshold = 22) %>%
    mutate(yearsMaxTemp = ifelse(is.na(yearsMaxTemp), 0, as.numeric(yearsMaxTemp))) %>%
    rename(yearsMaxTemp.22 = yearsMaxTemp) %>%
    dplyr::mutate(freqMaxTemp.22 = yearsMaxTemp.22 / length(unique(byfeatureidYear$year)))
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "yearsMaxTemp.22"] <- NA
  derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "freqMaxTemp.22"] <- NA
  derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
    dplyr::mutate(yearsMaxTemp.22 = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.22), 0, yearsMaxTemp.22),
                  freqMaxTemp.22 = ifelse(!is.na(meanMaxTemp) & is.na(freqMaxTemp.22), 0, freqMaxTemp.22))
  
  # Resistance to peak air temperature
  ## This probably makes the most sense during minimum flow periods but we don't have a sufficient flow model
  ## 60 or 90 days max air temp?
  # error if use standardized values rather than original scale
  # dOY.max.warm <- byfeatureidYear %>%
  #    mutate(warm.90 = rollsum(x = airTemp, 90, align = "right", fill = NA))
  #  dOY.max.warm <- dOY.max.warm %>%
  #   group_by(featureid, year, add = TRUE) %>%
  #  filter(warm.90 == max(warm.90, na.rm = T)) %>%
  # select(dOY)
  meanResist <- byfeatureidYear %>%
    #filter(dOY > dOY.max.warm$dOY - 90 & dOY <= dOY.max.warm$dOY) %>%
    dplyr::filter(dOY >= 152 & dOY < 244) %>% # clip to summer
    dplyr::mutate(absResid = abs(airTemp - tempPredicted)) %>%
    dplyr::summarise(resistance = sum(absResid)) %>%
    dplyr::summarise(meanResist = mean(resistance))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanResist, by = "featureid")
  rm(meanResist)
  
  
  # User broom and dplyr to get TS for each feature id but make sure it handles NA for entire featureid or entire fullDataSyncsets
  # Orange %>% group_by(Tree) %>% do(tidy(lm(age ~ circumference, fullDataSync=.)))
  
  # Thermal Sensitivity
  byfeatureid_complete <- byfeatureid %>%
    dplyr::filter(!is.na(tempPredicted) & !is.na(airTemp))
  
  group_count <- byfeatureid_complete %>%
    dplyr::summarise(count = n())
  
  if(nrow(byfeatureid_complete) >= 5) { # actually want to check for 5 obs within any group
    byfeatureid_complete <- left_join(byfeatureid_complete, group_count) %>%
      dplyr::filter(count >= 5)
    if(nrow(byfeatureid_complete) >= 5 & length(unique(byfeatureid_complete$featureid)) > 1) {
    TS <- byfeatureid_complete %>% do(broom::tidy(lm(tempPredicted ~ airTemp, data = .))) %>%
      dplyr::filter(term == "airTemp") %>%
      dplyr::select(featureid, TS = estimate)
    derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, TS, by = "featureid")
    rm(TS)
    } else {
      derivedfeatureidMetrics$TS <- NA_real_
    }
  } else {
    derivedfeatureidMetrics$TS <- NA_real_
  }

  
  # RMSE for each featureid (flag highest)
  bar <- byfeatureidYear %>%
    filter(!(is.na(temp) & !is.na(tempPredicted))) %>%
    mutate(error = temp - tempPredicted)
  
  if(dim(bar)[1] > 0) {
    meanRMSE <- bar %>%
      dplyr::summarise(RMSE = rmse(error)) %>%
      dplyr::summarise(meanRMSE = mean(RMSE, na.rm = T))
    derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, dplyr::select(meanRMSE, featureid, meanRMSE), by = "featureid")
  } else {
    derivedfeatureidMetrics$meanRMSE <- NA_real_
#     derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
#       dplyr::mutate(meanRMSE = NA) %>%
#       dplyr::mutate(meanRMSE = as.numeric(meanRMSE))
  }
  rm(bar)
  
  ############# ADD RMSE by Trend in addition to predicted with AR1 #########
  
  ########################
  
  #derived.site.metrics <- derivedfeatureidMetrics
  #rm(data)
  #rm(derivedfeatureidMetrics)
  #rm(foo)
  #gc()
  ################################
  
  # derived.site.metrics <- deriveMetrics(fullDataSync)
  
  #derived_metrics
#   if(exists("metrics") == FALSE) {
#     metrics <- derivedfeatureidMetrics
#     #print("no")
#   } else {
#     metrics <- rbind(metrics, derivedfeatureidMetrics)
#   }
  
  #saveRDS(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.RData"))
  #write.table(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.csv"), sep = ',', row.names = F, append = TRUE)
  
  metrics <- as.data.frame(derivedfeatureidMetrics)
  
#   dbClearResult(rs)
#   dbDisconnect(con)
#   dbUnloadDriver(drv)
  end.time <- Sys.time()
  cat(paste0(end.time, ": Finishing job ", i, " of ", n.loops, ".\n"), file = logFile_Finish, append = TRUE)
      
  return(metrics)
} # end dopar
stopCluster(cl)


low_july <- dplyr::filter(derived.site.metrics, meanJulyTemp < 5)
low_july

# can't remove catchments based on some featureid in the for loop - maybe because there are no catchments left in that loop causing an error in a function - therefore replace metrics with NA post hoc. Will need to get a list of featureid with these characteristics then replace the NA
# dplyr::filter(AreaSqKM >= 1 & AreaSqKM < 200 & allonnet < 70) # changed so don't deal with problematically small drainage areas (somre were 0.00006 sq km) - for loop didn't like this!!!!!!!!!


metrics.lat.lon <- left_join(featureid_lat_lon, derived.site.metrics, by = c('featureid')) # reverse this join or full join so get NA for all missing catchments? - doesn't seem to be working correctly yet - check again

saveRDS(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.RData"))
write.table(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.csv"), sep = ',', row.names = F)


gc()
#--------------------

################ TEST ###################

#     ### Calculate Trend
#     coef.list <- readRDS(paste0('coef.RData'))
#     load(paste0('tempDataSync.RData'))
#     cov_list <- readRDS(paste0('covariate_list.RData'))
#     
#     # make sure get rid of factors
#     coef.list$B.fixed <- coef.list$B.fixed %>%
#       dplyr::mutate(Parameter = as.character(Parameter),
#                     coef = as.character(coef))
#     
#     coef.list$B.site <- coef.list$B.site %>%
#       dplyr::mutate(Parameter = as.character(Parameter),
#                     site = as.character(site),
#                     coef = as.character(coef))
#     
#     coef.list$B.huc <- coef.list$B.huc %>%
#       dplyr::mutate(Parameter = as.character(Parameter),
#                     huc = as.character(huc),
#                     coef = as.character(coef))
#     
#     coef.list$B.year <- coef.list$B.year %>%
#       dplyr::mutate(Parameter = as.character(Parameter),
#                     year = as.character(year),
#                     coef = as.character(coef))
#     
#     coef.list$B.ar1 <- coef.list$B.ar1 %>%
#       dplyr::mutate(Parameter = as.character(Parameter),
#                     site = as.character(site))
#     
#     # get site effects
#     B.site <- coef.list$B.site %>%
#       dplyr::select(site, coef, mean) %>%
#       tidyr::spread(key = coef, value = mean) %>%
#       dplyr::select(one_of(c("site", cov.list$site.ef)))
#     site.names <- names(B.site[ , -1])
#    names(B.site) <- c("site", paste0(site.names, ".B.site"))
#     df <- merge(data, B.site, by = ("featureid" = "site"), all.x = T)
#     for(i in 2:length(names(B.site))) {
#       df[ , names(B.site[i])][is.na(df[ , names(B.site[i])])] <- colMeans(B.site[i])
#     }
#     
#     # get ar1 effects
#     B.ar1 <- coef.list$B.ar1 %>%
#       dplyr::select(site, B.ar1 = mean)
#     df <- merge(df, B.ar1, by = ("featureid" = "site"), all.x = T)
#     for(i in 2:length(names(B.ar1))) {
#       df[ , names(B.ar1[i])][is.na(df[ , names(B.ar1[i])])] <- colMeans(B.ar1[i])
#     }
#     
#     # get huc effects
#     B.huc <- coef.list$B.huc %>%
#       dplyr::select(huc, coef, mean) %>%
#       tidyr::spread(key = coef, value = mean) %>%
#       dplyr::select(one_of(c("huc", cov.list$huc.ef)))
#     huc.names <- names(B.huc[ , -1])
#     names(B.huc) <- c("huc", paste0(huc.names, ".B.huc"))
#     df <- merge(df, B.huc, by = ("featureid" = "huc"), all.x = T)
#     for(i in 2:length(names(B.huc))) {
#       df[ , names(B.huc[i])][is.na(df[ , names(B.huc[i])])] <- colMeans(B.huc[i])
#     }
#     
#     # get year effects
#     B.year <- coef.list$B.year %>%
#       dplyr::select(year, coef, mean) %>%
#       tidyr::spread(key = coef, value = mean) %>%
#       dplyr::select(one_of(c("year", cov.list$year.ef)))
#     year.names <- names(B.year[ , -1])
#     names(B.year) <- c("year", paste0(year.names, ".B.year"))
#     df <- merge(df, B.year, by = ("featureid" = "year"), all.x = T)
#     for(i in 2:length(names(B.year))) {
#       df[ , names(B.year[i])][is.na(df[ , names(B.year[i])])] <- colMeans(B.year[i])
#     }
#     
#     
#     df$trend <- NA
#     df$trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef))))))) +
#       rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, one_of(names(B.site[-1]))))) +
#       rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * as.matrix(select(df, one_of(names(B.huc[-1]))))) +
#       rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * as.matrix(select(df, one_of(names(B.year[-1])))))
#     
#     df <- dplyr::arrange(df, featureid, date)
#     
#     # Add B.ar1 to predictions
#     df <- mutate(df, prev.temp = c(NA, temp[(2:(nrow(data))) -1]))
#     df <- mutate(df, prev.trend = c(NA, trend[(2:nrow(data)) - 1]))
#     df <- mutate(df, prev.err = prev.temp - prev.trend)
#     df <- mutate(df, tempPredicted = trend)
#     df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), ]$prev.err
#     
#     


################# End TEST ##################



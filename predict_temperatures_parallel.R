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
library(zoo)
library(lubridate)
library(DataCombine) # for the slide function
library(RPostgreSQL)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

data_dir <- "localData_2015-07-09" 

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
                huc = as.character(HUC8)) %>%
  dplyr::rename(HUC12 = huc12)

# Get HUC8 - temporary for legacy code
featureid_huc8 <- df_huc %>%
  dplyr::select(featureid, HUC8, huc)


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
dbListConnections(drv)

gc()

#------------------loop through all catchments----------- 
# ?could speed up by excluding large drainage areas?

########## Set Up Parallel Processing ##########
  library(foreach)
  #library(doMC) # doesn't work well with connecting to the database
  library(doParallel)
  library(RPostgreSQL)
  
  # size of chunks
  chunk.size <- 20
  n.loops <- ceiling(n.catches / chunk.size)
  
  # set up parallel backend & make database connection available to all workers
  nc <- min(c(detectCores()-1, 15)) # use the maximum number of cores minus 1 or up to 15 because max 16 database connections
  cl <- makePSOCKcluster(nc)
  registerDoParallel(cl)
#   clusterEvalQ(cl, {
#     library(DBI)
#     library(RPostgreSQL)
#   })
  
  # setup to write out to monitor progress
  logFile = paste0(data_dir, "/log_file.txt")
  cat("Monitoring progress of prediction loop in parallel", file=logFile, append=FALSE, sep = "\n")
 
  ########## Run Parallel Loop ########## 
  # start loop
foreach(i = 1:n.loops, 
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
          #.export = c("derive_metrics_par")#,
          .export=ls(envir=globalenv())
  ) %dopar% {
    
    ########## Set up database connection ##########
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname='sheds', host='felek.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
    
    # write start of each iteration
    #sink("localData/log.txt", append = TRUE)
    cat(paste("Starting iteration", i, " of ", n.loops, "\n"), file = logFile, append = TRUE)
    
    
    #derived_metrics <- derive_metrics_par(i=i, chunk.size=chunk.size, catchmentid=catchmentid, springFallBPs=springFallBPs, tempDataSync=tempDataSync, df_covariates_upstream=df_covariates_upstream, featureid_huc8=featureid_huc8, featureid_lat_lon=featureid_lat_lon, featureid_site = featureid_site, coef.list=coef.list, cov.list=cov.list, var.names=var.names, con = con)
    
    ########## Set group of featureid to calculate for ##########
    n.catches <- length(catchmentid)
    k <- i*chunk.size
    if(k <= n.catches) {
      catches <- catchmentid[(1+(i-1)*chunk.size):k]
    } else {
      catches <- catchmentid[(1+(i-1)*chunk.size):n.catches]
    }
    catches_string <- paste(catches, collapse = ', ')
    
    # reconnect to database if lost
    #   if(isPostgresqlIdCurrent(con) == FALSE) {
    #     drv <- dbDriver("PostgreSQL")
    #     con <- dbConnect(drv, dbname='sheds', host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
    #   }
    
    ########## pull daymet records ##########
    qry_daymet <- paste0("SELECT featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe, (tmax + tmin) / 2.0 AS airTemp FROM daymet WHERE featureid IN (", catches_string, ") ;")
    rs <- dbSendQuery(con, statement = qry_daymet)
    climateData <- fetch(rs, n=-1)
    
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
      dplyr::mutate(dOY = yday(date)) %>%
      #dplyr::mutate(huc = huc8) %>%
      dplyr::filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>%
      dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp) %>%
      dplyr::filter(AreaSqKM < 400)
    
    # Order by group and date
    fullDataSync <- fullDataSync[order(fullDataSync$featureid, fullDataSync$year, fullDataSync$dOY),]
    
    # For checking the order of fullDataSync
    fullDataSync$count <- 1:length(fullDataSync$year)
    
    fullDataSync <- fullDataSync[order(fullDataSync$count),] # just to make sure fullDataSync is ordered for the slide function
    
    # moving means instead of lagged terms in the future
    fullDataSync <- fullDataSync %>%
      group_by(featureid, year) %>%
      arrange(featureid, year, dOY) %>%
      mutate(airTempLagged1 = lag(airTemp, n = 1, fill = NA),
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
             site = as.numeric(as.factor(featureid))) 
    
    fullDataSyncS <- stdCovs(x = fullDataSync, y = df_stds, var.names = var.names)
    
    fullDataSyncS <- addInteractions(fullDataSyncS)
    
    fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)
    
    fullDataSyncS <- predictTemp(data = fullDataSyncS, coef.list = coef.list, cov.list = cov.list, featureid_site = featureid_site)
    
    foo <- predictTemp(data = tempDataSyncS, coef.list = coef.list, cov.list = cov.list, featureid_site = featureid_site)
    
    if(FALSE) {
      ggplot(fullDataSyncS, aes(temp, tempPredicted)) + geom_point()
    }
    
    ################ TEST ###################
    
    ### Calculate Trend
    coef.list <- readRDS(paste0('coef.RData'))
    load(paste0('tempDataSync.RData'))
    cov_list <- readRDS(paste0('covariate_list.RData'))
    
    # make sure get rid of factors
    coef.list$B.fixed <- coef.list$B.fixed %>%
      dplyr::mutate(Parameter = as.character(Parameter),
                    coef = as.character(coef))
    
    coef.list$B.site <- coef.list$B.site %>%
      dplyr::mutate(Parameter = as.character(Parameter),
                    site = as.character(site),
                    coef = as.character(coef))
    
    coef.list$B.huc <- coef.list$B.huc %>%
      dplyr::mutate(Parameter = as.character(Parameter),
                    huc = as.character(huc),
                    coef = as.character(coef))
    
    coef.list$B.year <- coef.list$B.year %>%
      dplyr::mutate(Parameter = as.character(Parameter),
                    year = as.character(year),
                    coef = as.character(coef))
    
    coef.list$B.ar1 <- coef.list$B.ar1 %>%
      dplyr::mutate(Parameter = as.character(Parameter),
                    site = as.character(site))
    
    # get site effects
    B.site <- coef.list$B.site %>%
      dplyr::select(site, coef, mean) %>%
      tidyr::spread(key = coef, value = mean) %>%
      dplyr::select(one_of(c("site", cov.list$site.ef)))
    site.names <- names(B.site[ , -1])
   names(B.site) <- c("site", paste0(site.names, ".B.site"))
    df <- merge(data, B.site, by = ("featureid" = "site"), all.x = T)
    for(i in 2:length(names(B.site))) {
      df[ , names(B.site[i])][is.na(df[ , names(B.site[i])])] <- colMeans(B.site[i])
    }
    
    # get ar1 effects
    B.ar1 <- coef.list$B.ar1 %>%
      dplyr::select(site, B.ar1 = mean)
    df <- merge(df, B.ar1, by = ("featureid" = "site"), all.x = T)
    for(i in 2:length(names(B.ar1))) {
      df[ , names(B.ar1[i])][is.na(df[ , names(B.ar1[i])])] <- colMeans(B.ar1[i])
    }
    
    # get huc effects
    B.huc <- coef.list$B.huc %>%
      dplyr::select(huc, coef, mean) %>%
      tidyr::spread(key = coef, value = mean) %>%
      dplyr::select(one_of(c("huc", cov.list$huc.ef)))
    huc.names <- names(B.huc[ , -1])
    names(B.huc) <- c("huc", paste0(huc.names, ".B.huc"))
    df <- merge(df, B.huc, by = ("featureid" = "huc"), all.x = T)
    for(i in 2:length(names(B.huc))) {
      df[ , names(B.huc[i])][is.na(df[ , names(B.huc[i])])] <- colMeans(B.huc[i])
    }
    
    # get year effects
    B.year <- coef.list$B.year %>%
      dplyr::select(year, coef, mean) %>%
      tidyr::spread(key = coef, value = mean) %>%
      dplyr::select(one_of(c("year", cov.list$year.ef)))
    year.names <- names(B.year[ , -1])
    names(B.year) <- c("year", paste0(year.names, ".B.year"))
    df <- merge(df, B.year, by = ("featureid" = "year"), all.x = T)
    for(i in 2:length(names(B.year))) {
      df[ , names(B.year[i])][is.na(df[ , names(B.year[i])])] <- colMeans(B.year[i])
    }
    
    
    df$trend <- NA
    df$trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef))))))) +
      rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, one_of(names(B.site[-1]))))) +
      rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * as.matrix(select(df, one_of(names(B.huc[-1]))))) +
      rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * as.matrix(select(df, one_of(names(B.year[-1])))))
    
    df <- dplyr::arrange(df, featureid, date)
    
    # Add B.ar1 to predictions
    df <- mutate(df, prev.temp = c(NA, temp[(2:(nrow(data))) -1]))
    df <- mutate(df, prev.trend = c(NA, trend[(2:nrow(data)) - 1]))
    df <- mutate(df, prev.err = prev.temp - prev.trend)
    df <- mutate(df, tempPredicted = trend)
    df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), ]$prev.err
    
    
    
    
    ################# End TEST ##################
    
    fullDataSync <- left_join(fullDataSync, select(fullDataSyncS, featureid, date, tempPredicted), by = c("featureid", "date"))
    
    mean.pred <- mean(fullDataSync$tempPredicted, na.rm = T)
    
    if(mean.pred == "NaN") {
      cat(paste0(i, " of ", n.loops, " loops has no predicted temperatures"))
    } 
    derived.site.metrics <- deriveMetrics(fullDataSync)
    
    return(derived.site.metrics)
    
    dbDisconnect(con)
    
    #derived_metrics
    
    metrics.lat.lon <- left_join(featureid_lat_lon, derived_metrics, by = c('featureid')) # reverse this join or full join so get NA for all missing catchments? - doesn't seem to be working correctly yet - check again
    
    saveRDS(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.RData"))
    write.table(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.csv"), sep = ',', row.names = F)
    
  } # end dopar
  
  stopCluster(cl)
  
  dbClearResult(res = rs)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  dbListConnections(drv)
  
  clusterEvalQ(cl, {
       dbDisconnect(con)
     })
  
  gc()
#--------------------


#derived.site.metrics.clean <- na.omit(derived.site.metrics)

# add flags once have all catchment RMSE
# Flag based on RMSE > 95%
# add an ifelse for whether data is present or not #######################
#derivedfeatureidMetrics <- mutate(derivedfeatureidMetrics, flag = ifelse(meanRMSE > quantile(derivedfeatureidMetrics$meanRMSE, probs = c(0.95), na.rm=TRUE), "Flag", ""))

metrics.lat.lon <- left_join(featureid_lat_lon, metrics, by = c('featureid')) # reverse this join or full join so get NA for all missing catchments? - doesn't seem to be working correctly yet - check again

saveRDS(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.RData"))
write.table(metrics.lat.lon, file = paste0(data_dir, "/derived_site_metrics.csv"), sep = ',', row.names = F)

#}




test1 <- derive_metrics_par(i=500, chunk.size=chunk.size, catchmentid=catchmentid, springFallBPs=springFallBPs, tempDataSync=tempDataSync, df_covariates_upstream=df_covariates_upstream, featureid_huc8=featureid_huc8, featureid_lat_lon=featureid_lat_lon, coef.list=coef.list, cov.list=cov.list, var.names=var.names)

test2 <- derive_metrics_par(i=1, chunk.size=chunk.size, catchmentid=catchmentid, springFallBPs=springFallBPs, tempDataSync=tempDataSync, df_covariates_upstream=df_covariates_upstream, featureid_huc8=featureid_huc8, featureid_lat_lon=featureid_lat_lon, featureid_site=featureid_site, coef.list=coef.list, cov.list=cov.list, var.names=var.names)



rm(list = ls())
gc()


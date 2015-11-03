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

data_dir <- "localData_2015-09-28" 

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
chunk.size <- 4
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
  dbClearResult(rs)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  ########## Set up database connection ##########
#   drv <- dbDriver("PostgreSQL")
#   con <- dbConnect(drv, dbname='sheds', host='felek.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
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
  
  fullDataSyncS <- predictTemp(catches_string=catches_string, springFallBPs=springFallBPs, df_covariates_upstream=df_covariates_upstream, tempDataSync=tempDataSync, featureid_lat_lon=featureid_lat_lon, featureid_huc8=featureid_huc8)
  
  dbClearResult(rs)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  
  fullDataSync <- data.frame(unclass(fullDataSync))
  
  fullDataSync2 <- left_join(fullDataSync, select(fullDataSyncS, featureid, date, trend, tempPredicted))
  
  metrics <- deriveMetrics(fullDataSync = fullDataSync2)
  
  end.time <- Sys.time()
  cat(paste0(end.time, ": Finishing job ", i, " of ", n.loops, ".\n"), file = logFile_Finish, append = TRUE)
      
  return(metrics)
} # end dopar
stopCluster(cl)

# filter
derived.site.metrics <- left_join(derived.site.metrics, df_covariates_upstream) %>%
  dplyr::filter(AreaSqKM >= 1 & AreaSqKM < 200 & allonnet < 70) # changed so don't deal with problematically small drainage areas (somre were 0.00006 sq km) - for loop didn't like this!!!!!!!!!

# look for errors
low_july <- dplyr::filter(derived.site.metrics, meanJulyTemp < 5)
low_july

hi_july <- dplyr::filter(derived.site.metrics, meanJulyTemp > 45)
dim(hi_july)
summary(hi_july)

absurd_july <- dplyr::filter(derived.site.metrics, meanJulyTemp > 500)
summary(absurd_july) # there is no obvious landscape characteristic that causes unrealistic predictions. Therefore it must be something either about missing covariates or weather covariates (likely when in complex interactions). I can't output all predictions but I should be able to look through at least a subset based on featureid. It would be easiest if I made the above code into a function now that it's "working".

sites_pred <- dplyr::filter(derived.site.metrics, !is.na(meanJulyTemp))
dim(sites_pred)

# add mean min and max air temp and precip to derived metrics just for comparison and error checking


################### PROBLEM ################
# if allonnet is very large (maybe > 75% of drainage area) the predictions are probably non-sense 
##########################

################### PROBLEM #################
# 2-day precip as large as 210 - not sure if this is realistic and if so it might be outside the scope of our predictions
##################################

# need to check for errors during prediction stage and flag and/or throw out those time series


# can't remove catchments based on some featureid in the for loop - maybe because there are no catchments left in that loop causing an error in a function - therefore replace metrics with NA post hoc. Will need to get a list of featureid with these characteristics then replace the NA
# dplyr::filter(AreaSqKM >= 1 & AreaSqKM < 200 & allonnet < 70) # changed so don't deal with problematically small drainage areas (somre were 0.00006 sq km) - for loop didn't like this!!!!!!!!!
featureid_bad <- df_covariates_upstream %>%
  dplyr::filter(AreaSqKM < 1 | AreaSqKM >= 200 | allonnet >= 70)

derived.site.metrics <- derived.site.metrics %>%
  dplyr::filter(!(featureid %in% featureid_bad$featureid))

metrics.lat.lon <- featureid_lat_lon %>%
  left_join(derived.site.metrics, by = c('featureid')) # reverse this join or full join so get NA for all missing catchments? - doesn't seem to be working correctly yet - check again

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



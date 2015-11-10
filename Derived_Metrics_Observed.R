# Predicts daily stream temperatures and summarizes the derived metrics

# usage: RScript test_1.R <working directory> <inputs.json> <outputs.json>

rm(list=ls())
gc()
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


########## Temporary save for testing ########## 
# temporary save entire environment so don't have to pull from dataframe for testing
load(file.path(getwd(), paste0(data_dir, "/db_pull_for_predictions.RData")))


catchmentid <- unique(c(unique(tempDataSyncS$featureid), unique(tempDataSyncValidS$featureid)))
n.catches <- length(catchmentid)

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
nc <- min(c(detectCores()-1, 16)) # use the maximum number of cores minus 1 or up to 15 because max 16 database connections - changed to 12 since Evan also often using osensei on multiple cores
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
                                #.export=ls(envir=globalenv(),
                                #          "indexDeployments")# shouldn't be needed after update package
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
  
  data_list <- prepData(catches_string=catches_string, springFallBPs=springFallBPs, df_covariates_upstream=df_covariates_upstream, tempDataSync=tempDataSync, featureid_lat_lon=featureid_lat_lon, featureid_huc8=featureid_huc8, rand_ids=rand_ids)
  
  fullDataSyncS <- predictTemp(fullDataSyncS = data_list$fullDataSyncS, coef.list= coef.list, rand_ids=rand_ids)
  
  dbClearResult(rs)
  dbDisconnect(con)
  #dbUnloadDriver(drv)
  
  fullDataSync <- left_join(data_list$fullDataSync, dplyr::select(fullDataSyncS, featureid, date, trend, tempPredicted))
  
  #fullDataSync <- data.frame(unclass(fullDataSync), stringsAsFactors = FALSE)
  
  #fullDataSync2 <- left_join(fullDataSync, dplyr::select(fullDataSyncS, featureid, date, trend, tempPredicted))
  
  metrics <- deriveMetrics(fullDataSync = fullDataSync)
  
  end.time <- Sys.time()
  cat(paste0(end.time, ": Finishing job ", i, " of ", n.loops, ".\n"), file = logFile_Finish, append = TRUE)
  
  return(metrics)
} # end dopar
stopCluster(cl)

saveRDS(derived.site.metrics, file = paste0(data_dir, "/derived_site_metrics_observed.RData"))


gc()


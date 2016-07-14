
data_dir <- "localData_2015-11-20" 

load(file.path(getwd(), paste0(data_dir, "/db_pull_for_predictions.RData")))

coef.list.iters <- readRDS(paste0(data_dir, "/coef_iters.RData"))

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
                                .export = c("indexDeployments", "deriveMetrics") # shouldn't be needed after update package
                                #.export = c("derive_metrics_par")#,
                                #.export=ls(envir=globalenv(),
                                #          "indexDeployments")# shouldn't be needed after update package
) %dopar% {
  
  #for(i in 1:n.loops) {
  dbClearResult(rs)
  dbDisconnect(con)
  #dbUnloadDriver(drv)
  ########## Set up database connection ##########
  #   drv <- dbDriver("PostgreSQL")
  #   con <- dbConnect(drv, dbname='sheds', host='osensei.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
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
  
  data_list <- prepData(catches_string=catches_string, springFallBPs=springFallBPs, df_covariates_upstream=df_covariates_upstream, tempDataSync=tempDataSync, featureid_lat_lon=featureid_lat_lon, featureid_huc8=featureid_huc8, rand_ids=rand_ids, df_stds = df_stds)
  
  fullDataSyncS <- predictTemp(data = data_list$fullDataSyncS, coef.list= coef.list, cov.list = cov.list, rand_ids=rand_ids)
  
  dbClearResult(rs)
  dbDisconnect(con)
  #dbUnloadDriver(drv)
  
  fullDataSync <- left_join(data_list$fullDataSync, dplyr::select(fullDataSyncS, featureid, date, trend, tempPredicted))
  
  #fullDataSync <- data.frame(unclass(fullDataSync), stringsAsFactors = FALSE)
  
  #fullDataSync2 <- left_join(fullDataSync, dplyr::select(fullDataSyncS, featureid, date, trend, tempPredicted))
  
  metrics <- deriveMetrics(data = fullDataSync)
  
  end.time <- Sys.time()
  cat(paste0(end.time, ": Finishing job ", i, " of ", n.loops, ".\n"), file = logFile_Finish, append = TRUE)
  
  return(metrics)
} # end dopar
stopCluster(cl)



gc()


for(i in 1:n.iter) {
  fixed.ef <- as.numeric(B.0[i, ]) # separate out the iteration or do for mean/median
  # add specific random effects to the dataframe
  df <- data %>%
    dplyr::select(-sitef) %>%
    left_join(rand_ids$df_site) %>%
    left_join(rand_ids$df_huc) %>%
    left_join(rand_ids$df_year)
  
  
  
  df <- left_join(df, B.site.list[[i]], by = "sitef")
  df <- left_join(df, B.huc.list[[i]], by = "hucf") # problem with validation data, need to use the mean when huc don't match
  df <- left_join(df, B.year.list[[i]], by = "yearf")
  
  
  for (j in 2:length(names(B.site.list[[i]]))) {
    df[, names(B.site.list[[i]][j])][is.na(df[, names(B.site.list[[i]][j])])] <- colMeans(B.site.list[[i]][j])
  }
  for (j in 2:length(names(B.huc.list[[i]]))) {
    df[, names(B.huc.list[[i]][j])][is.na(df[, names(B.huc.list[[i]][j])])] <- colMeans(B.huc.list[[i]][j])
  }
  for (j in 2:length(names(B.year.list[[i]]))) {
    df[, names(B.year.list[[i]][j])][is.na(df[, names(B.year.list[[i]][j])])] <- colMeans(B.year.list[[i]][j])
  }
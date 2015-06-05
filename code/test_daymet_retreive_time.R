
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
#library(conteStreamTemperature)

#-------------get list of unique catchments to loop through-------------
# connect to database source
db <- src_postgres(dbname='conte_dev', host='ecosheds.org', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))


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

n.catches <- length(catchmentid)


load("localData/db_pull_for_predictions.RData")

#---------------------sequential----------------
db <- src_postgres(dbname='sheds', host='ecosheds.org', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))


chunk.size <- c(1, 10, 100)
time <- matrix(NA, length(chunk.size), ncol = length(proc.time()))
for(j in 1:length(chunk.size)) {
  n.loops <- ceiling(n.catches / chunk.size[j])
  iters <- 1000/chunk.size[j]
  
  start_time <- proc.time()
  for(i in 1:iters) {
    if(isPostgresqlIdCurrent(db$con) == FALSE) {
      print("Reconnecting to the database")
      db <- src_postgres(dbname='sheds', host='ecosheds.org', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
    }
    
    k <- i*chunk.size[j]
    catches <- catchmentid[(1+(i-1)*chunk.size[j]):k]
    
    if(chunk.size[j] == 1) {
      qry_daymet <- tbl(db, 'daymet') %>%
        dplyr::filter(featureid == catches) %>%
        dplyr::mutate(airTemp = (tmax + tmin)/2)
    } else {
      qry_daymet <- tbl(db, 'daymet') %>%
        dplyr::filter(featureid %in% catches) %>%
        dplyr::mutate(airTemp = (tmax + tmin)/2)
    }
    climateData <- collect(qry_daymet)
    
  }
  time[j, ] <- proc.time() - start_time
}

colnames(time) <- names(proc.time())

dbDisconnect(db$con)

#--------------------parallel----------------
library(foreach)
#library(doMC) # doesn't work well with connecting to the database
library(doParallel)
library(RPostgreSQL)

chunk.size <- c(1, 10, 100)
time.par <- matrix(NA, length(chunk.size), ncol = length(proc.time()))

# setup log for output of which scenario is running
writeLines(c(""), "localData/scenario_log.txt")

# loop of scenarios to test pulling different numbers of records at a time
for(j in 1:length(chunk.size)) {
  
  # write start of each iteration
  sink("localData/log.txt", append = TRUE)
  cat(paste("Starting scenario", j, " of ", length(chunk.size), "\n"))
  
  # determine number of iterations for scenario
  n.loops <- ceiling(n.catches / chunk.size[j])
  iters <- 1000/chunk.size[j]
  
  # start timer
  start_time <- proc.time()
  
  # set up parallel backend & make database connection available to all workers
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(DBI)
    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname='sheds', host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  })
  
  # setup to write out to monitor progress
  writeLines(c(""), "localData/log.txt")
  
  # start loop
  foreach(i = 1:iters, .inorder=FALSE,
          .packages=c("DBI", "RPostgreSQL")) %dopar% {
            
            # write start of each iteration
            sink("localData/log.txt", append = TRUE)
            cat(paste("Starting iteration", i, " of ", iters, "\n"))
            
            # reconnect to database if lost
            if(isPostgresqlIdCurrent(con) == FALSE) {
              drv <- dbDriver("PostgreSQL")
              con <- dbConnect(drv, dbname='sheds', host='ecosheds.org', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
            }
            
            k <- i*chunk.size[j]
            catches <- catchmentid[(1+(i-1)*chunk.size[j]):k]
            catches_string <- paste(catches, collapse = ', ')
            
            qry_daymet <- paste0("SELECT featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe, (tmax + tmin) / 2.0 AS airTemp FROM daymet WHERE featureid IN (", catches_string, ") ;")
            
            rs <- dbSendQuery(con, statement = qry_daymet)
            climateData <- fetch(rs, n=-1)
          } # end loop i
  
  # disconnect from database
  clusterEvalQ(cl, {
    dbDisconnect(con)
    dbUnloadDriver(drv)
  })
  
  # collect timer
  time.par[j, ] <- proc.time() - start_time
  
}
colnames(time.par) <- names(proc.time())
stopCluster(cl)
gc()
time.par



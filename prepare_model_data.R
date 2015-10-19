# Prepare model input dataset (tempDataSync)
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript breakpoints.R <input temperatureData rdata> <input climateData csv> <input springFallBPs rdata> <input excludeID csv> <output tempDataSync rdata>
# example: $ Rscript prepare_model_data.R ./temperatureData.RData ./daymet_results.csv ./springFallBPs.RData ./sample_locations_50m ./tempDataSync.RData
#
# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(ggmcmc) # load before dplyr so later plyr doesn't override dplyr
library(dplyr)
library(DataCombine) # for the slide function
library(zoo) # for rollapply function
library(lubridate)
library(stringr)
library(devtools)
install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(jsonlite)

config <- fromJSON('model_config.json')

# validate = TRUE # get rid of this once model_config.json is ready

data_dir <- "localData_2015-09-28" 

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# until running as a bash script add the files here
if(length(args) < 1) {
  args <- c(paste0(data_dir, "/temperatureData.RData"), paste0(data_dir, "/daymet_results.csv"), paste0(data_dir, "/covariateData.RData"), paste0(data_dir, "/springFallBPs.RData"), paste0(data_dir, "/sample_locations_50m.csv"), paste0(data_dir, "/tempDataSync.RData"))
}


######### add wd argument

temperatureData_file <- args[1]
if (!file.exists(temperatureData_file)) {
  stop(paste0('Could not find temperatureData binary file: ', temperatureData_file))
}
temperatureData <- readRDS(temperatureData_file)

climateData_file <- args[2]
if (!file.exists(climateData_file)) {
  stop(paste0('Could not find climateData binary file: ', climateData_file))
}
climateData <- fread(climateData_file, header = TRUE, sep = ",")

covariateData_file <- args[3]
if (!file.exists(covariateData_file)) {
  stop(paste0('Could not find covariateData binary file: ', covariateData_file))
}
covariateData <- readRDS(covariateData_file)

springFallBPs_file <- args[4]
if (!file.exists(springFallBPs_file)) {
  stop(paste0('Could not find springFallBPs binary file: ', springFallBPs_file))
}
springFallBPs <- readRDS(springFallBPs_file)

exclude_file <- args[5]
if (!file.exists(exclude_file)) {
  stop(paste0('Could not find exclude binary file: ', exclude_file))
}
exclude_sites <- fread(exclude_file, header = TRUE, stringsAsFactors = FALSE, sep = ",")

output_file <- args[6]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

tempData <- climateData %>%
  dplyr::mutate(site = as.numeric(as.factor((featureid))),
                dOY = yday(date),
                airTemp = (tmax + tmin) / 2)

#------ REMOVE SITES WITHIN 50 m OF AN IMPOUNDMENT - A. Roy unpublished data --------#

exclude_ids <- as.numeric(gsub(",", "", unique(exclude_sites$catchment_id)))
tempData <- tempData %>%
  dplyr::filter(!(featureid %in% exclude_ids))

#-------------------------------------------------------------------------------------

# Order by group and date
tempData <- tempData[order(tempData$site,tempData$year,tempData$dOY), ]

# For checking the order of tempDataSync
tempData$count <- 1:length(tempData$year)

tempData <- tempData[order(tempData$count),] # just to make sure tempDataSync is ordered for the slide function

# airTemp
#tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -1, NewVar='airTempLagged1')
#tempDataSync <- slide(tempDataSync, Var = "airTemp", GroupVar = "site", slideBy = -2, NewVar='airTempLagged2')

# prcp
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -1, NewVar='prcpLagged1')
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -2, NewVar='prcpLagged2')
#tempDataSync <- slide(tempDataSync, Var = "prcp", GroupVar = "site", slideBy = -3, NewVar='prcpLagged3')

# moving means instead of lagged terms in the future
tempData <- tempData %>%
  group_by(site, year) %>%
  arrange(site, year, dOY) %>%
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


# masterData <- masterData[, c("agency", "date", "AgencyID", "year", "site", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")]

#tempData <- left_join(tempData, covariateData)


#covariateData <- readStreamTempData(timeSeries=TRUE, covariates=TRUE, dataSourceList=dataSource, fieldListTS=fields, fieldListCD='ALL', directory=dataInDir)

tempDataBP <- temperatureData %>%
  dplyr::filter(!is.na(featureid)) %>%
  left_join(dplyr::mutate(data.frame(unclass(tempData)), date = as.Date(date))) %>%
  left_join(covariateData, by = c("featureid")) %>%
  dplyr::mutate(site = as.character(featureid),
                impoundArea = AreaSqKM * allonnet)

springFallBPs$site <- as.character(springFallBPs$site)

# Join with break points
tempDataBP <- left_join(tempDataBP, springFallBPs)

#------------ evaluate break points ---------------
bp_test <- TRUE
if(bp_test) {
  #str(tempDataBP)
  #str(tempDataSync)
  df <- tempDataBP %>%
    dplyr::select(featureid, year, date, dOY, temp, airTemp, finalSpringBP, finalFallBP, sourceSpringBP, sourceFallBP) 
  
  ggplot(df, aes(finalSpringBP)) + geom_histogram()
  ggplot(df, aes(finalFallBP)) + geom_histogram()
  
  foo <- df %>%
    dplyr::filter(finalSpringBP < 50)
  
  str(foo)
  unique(foo$featureid)
  
  ggplot(foo, aes(dOY, temp)) + geom_point(colour = "blue") + geom_line(colour = "blue") + geom_point(aes(dOY, airTemp)) + geom_vline(aes(finalSpringBP), colour = "red") + geom_vline(aes(finalFallBP), colour = "red") + facet_wrap(facets = ~ featureid + year)
  
  # manually assign springBP to those with clear errors in assignment
  tempDataBP[which(tempDataBP$featureid == "752852" & tempDataBP$year == 2012), "finalSpringBP"] <- mean(tempDataBP[which(tempDataBP$featureid == "752852" & tempDataBP$year != 2012), "finalSpringBP"]$finalSpringBP, na.rm = TRUE)
  tempDataBP[which(tempDataBP$featureid == "752852" & tempDataBP$year == 2012), "sourceSpringBP"] <- "site mean"
  
  tempDataBP[which(tempDataBP$featureid == "756672" & tempDataBP$year == 2007), "finalSpringBP"] <- mean(tempDataBP[which(tempDataBP$featureid == "756672" & tempDataBP$year != 2007), "finalSpringBP"]$finalSpringBP, na.rm = TRUE)
  tempDataBP[which(tempDataBP$featureid == "756672" & tempDataBP$year == 2007), "sourceSpringBP"] <- "site mean"
  
  tempDataBP[which(tempDataBP$featureid == "834122" & tempDataBP$year == 2000), "finalSpringBP"] <- mean(tempDataBP[which(tempDataBP$featureid == "834122" & tempDataBP$year != 2000), "finalSpringBP"]$finalSpringBP, na.rm = TRUE)
  tempDataBP[which(tempDataBP$featureid == "834122" & tempDataBP$year == 2000), "sourceSpringBP"] <- "site mean"
  
  # visually inspect fall BP at edge of year
  foo <- df %>%
    dplyr::filter(finalFallBP >= 340)
  
  str(foo)
  unique(foo$featureid)
  
  for(i in 1:length(unique(foo$featureid))) {
    bar <- dplyr::filter(foo, featureid == unique(foo$featureid)[i])
  g <- ggplot(bar, aes(dOY, temp)) + geom_point(colour = "blue") + geom_line(colour = "blue") + geom_point(aes(dOY, airTemp))  + geom_vline(aes(xintercept = bar$finalFallBP[1]), colour = "red")
  print(g)
} # all look reasonable
  
} # end if statement for BP_test
#---------------------------------

# Clip to syncronized season
tempDataSync <- dplyr::filter(tempDataBP, dOY >= finalSpringBP & dOY <= finalFallBP)

# check very low temperatures
foo <- tempDataSync %>%
  dplyr::filter(temp <= 1) %>%
  dplyr::select(featureid, year, date, dOY, temp, airTemp, finalSpringBP, finalFallBP, sourceSpringBP, sourceFallBP) 

str(foo)
unique(foo$featureid)

for(i in 1:length(unique(foo$featureid))) {
  bar <- dplyr::filter(foo, featureid == unique(foo$featureid)[i])
  g <- ggplot(bar, aes(dOY, temp)) + geom_point(colour = "blue") + geom_line(colour = "blue") + geom_point(aes(dOY, airTemp))  + geom_vline(aes(xintercept = bar$finalSpringBP[1]), colour = "red")
  print(g)
} # all look reasonable


# Filter by Drainage area
tempDataSync <- tempDataSync %>%
  dplyr::filter(AreaSqKM >= 1 & AreaSqKM < 200 & allonnet < 70) %>%
  dplyr::filter(!is.na(impoundArea))

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
               "impoundArea",
               "surfcoarse", 
               "srad", 
               "dayl", 
               "swe")

featureids <- unique(tempDataSync$featureid)

# connect to database source
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_huc12 <- tbl(db, 'catchment_huc12') %>%
  dplyr::filter(featureid %in% featureids)

df_huc <- tbl_huc12 %>%
  dplyr::collect() %>%
  dplyr::mutate(HUC4=as.character(str_sub(huc12, 1, 4)),
                HUC8=as.character(str_sub(huc12, 1, 8)),
                HUC10=as.character(str_sub(huc12, 1, 10)),
                huc = as.character(huc12)) %>%
  dplyr::rename(HUC12 = huc12)

tempDataSync <- tempDataSync %>%
  dplyr::left_join(df_huc)

tempDataSync <- tempDataSync %>%
  dplyr::filter(!is.na(prcp),
                !is.na(prcp30),
                !is.na(airTemp),
                !is.na(agriculture),
                !is.na(alloffnet),
                !is.na(surfcoarse))
#dplyr::select()


keep_columns <- c("featureid"
                  , "date"
                  , "year"
                  , "airTemp"
                  , "temp"
                  , "tempMax"
                  , "tempMin"
                  #, "n_obs"
                  , "tmax"
                  , "tmin"
                  , "prcp"
                  , "dayl"
                  , "srad"
                  , "swe"
                  , "site"
                  , "dOY"
                  , "airTemp"
                  , "temp7p"
                  #, "count"
                  , "prcp2"
                  , "prcp7"
                  , "prcp30"
                  , "latitude"
                  , "longitude"
                  , "agriculture"
                  , "herbaceous"
                  , "allonnet"
                  , "alloffnet"
                  , "AreaSqKM"
                  , "impoundArea"
                  , "developed"
                  , "devel_hi"
                  , "elevation"
                  , "forest"
                  , "impervious"
                  , "openonnet"
                  , "percent_sandy"
                  , "surfcoarse"
                  , "finalSpringBP"
                  , "finalFallBP"
                  , "tree_canopy"
                  , "HUC4"
                  , "HUC8"
                  , "HUC10"
                  , "HUC12"
                  , "huc")

tempDataSync <- dplyr::select(tempDataSync, one_of(keep_columns))

tempDataSync <- as.data.frame(unclass(tempDataSync), stringsAsFactors = FALSE)

### Separate data for fitting (training) and validation
#If validating:
if (config[['validate']]) {
  validate.frac.sites <- 0.15
  validate.frac.huc <- 0.10
  
  # leave out sites and hucs at random
  n.fit.sites <- floor(length(unique(tempDataSync$site)) * (1 - validate.frac.sites))
  n.fit.hucs <- floor(length(unique(tempDataSync$huc)) * (1 - validate.frac.huc))
  
  # leave out especially warm year (summer) across the regional
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(airTemp.mean = mean(airTemp))
  ggplot(foo, aes(year, airTemp.mean)) + geom_point() + geom_smooth() # 1999, 2005, 2010 all high tmean summers
  
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(tmax.mean = mean(tmax))
  ggplot(foo, aes(year, tmax.mean)) + geom_point() + geom_smooth() # 1995, 1999, 2005, 2010 all high tmax summers
  
  foo <- tempData %>%
    group_by(year) %>%
    dplyr::filter(dOY > 150 & dOY <245) %>%
    dplyr::summarise(tmin.mean = mean(tmin))
  ggplot(foo, aes(year, tmin.mean)) + geom_point() + geom_smooth() # 2005, 2006, 2010 all high tmin summers
  
  
  # random TO KEEP
  set.seed(2346)
  site.fit <- sample(unique(tempDataSync$site), n.fit.sites, replace = FALSE) # select sites to hold back for testing 
  huc.fit <- sample(unique(tempDataSync$huc), n.fit.hucs, replace = FALSE) # select hucs to hold back for testing 
  year.valid <- "2010"
  
  tempDataSyncValid <- tempDataSync %>%
    dplyr::filter(!(site %in% site.fit) | !(huc %in% huc.fit) | year == year.valid) # data for validation
  
  #featureid_date <- paste0(tempDataSync$featureid, "_", tempDataSync$date)
  #valid_set <- unique(tempDataSyncValid$featureid)
  
  tempDataSync <- tempDataSync %>%
    dplyr::filter(site %in% site.fit & huc %in% huc.fit & year != year.valid)   # data for model fitting (calibration)
  
  print(paste0(round(nrow(tempDataSyncValid)/(nrow(tempDataSync) + nrow(tempDataSyncValid))*100, digits = 1), "% of data points held out for validation"))
  
  
  #tempDataSyncValidS <- stdCovs(x = tempDataSyncValid, y = tempDataSync, var.names = var.names)
  
  ########## Means and SDs for Standardization ###########
  means <- NULL
  stdevs <- NULL
  for(i in 1:length(var.names)) {
    means[i] <- mean(tempDataSync[, var.names[i]], na.rm = T)
    stdevs[i] <- sd(tempDataSync[, var.names[i]], na.rm = T)
  }
  df_stds <- data.frame(var.names, means, stdevs, stringsAsFactors = FALSE)
  
  tempDataSyncValidS <- stdCovs(x = tempDataSyncValid, y = df_stds, var.names = var.names)
  #############
  
  tempDataSyncValidS <- indexDeployments(tempDataSyncValidS, regional = TRUE)
  firstObsRowsValid <- createFirstRows(tempDataSyncValidS)
  evalRowsValid <-createEvalRows(tempDataSyncValidS)
  tempDataSyncValidS <- addInteractions(tempDataSyncValidS)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  df_site <- data.frame(site = unique(tempDataSyncS$featureid))
  df_site$sitef <- seq(1, nrow(df_site), by = 1)
  df_huc <- data.frame(huc = unique(tempDataSyncS$huc))
  df_huc$hucf <- seq(1, nrow(df_huc), by = 1)
  df_year <- data.frame(year = unique(tempDataSyncS$year))
  df_year$yearf <- seq(1, nrow(df_year), by = 1)
  J <- nrow(df_site)
  M <- nrow(df_huc)
  Ti <- nrow(df_year)
  
  rand_ids <- list(df_site = df_site, df_huc = df_huc, df_year = df_year, J = J, M = M, Ti = Ti)
  
  rand_ids$df_site$site <- as.character(rand_ids$df_site$site)
  rand_ids$df_huc$huc <- as.character(rand_ids$df_huc$huc)
  
  tempDataSyncS <- tempDataSyncS %>%
    dplyr::select(-sitef) %>%
    dplyr::left_join(rand_ids$df_site) %>%
    dplyr::left_join(rand_ids$df_huc) %>%
    dplyr::left_join(rand_ids$df_year)
  
  save(tempDataSync, tempDataSyncS, tempDataSyncValid, tempDataSyncValidS, firstObsRows, evalRows, firstObsRowsValid, evalRowsValid, var.names, df_stds, rand_ids, file = output_file)
  
} else {
  #tempDataSyncValid <- NULL
  
  means <- NULL
  stdevs <- NULL
  for(i in 1:length(var.names)) {
    means[i] <- mean(tempDataSync[, var.names[i]], na.rm = T)
    stdevs[i] <- sd(tempDataSync[, var.names[i]], na.rm = T)
  }
  
  df_stds <- data.frame(var.names, means, var.names, stdevs, stringsAsFactors = FALSE)
  
  tempDataSyncS <- stdCovs(x = tempDataSync, y = df_stds, var.names = var.names)
  tempDataSyncS <- addInteractions(tempDataSyncS)
  tempDataSyncS <- indexDeployments(tempDataSyncS, regional = TRUE)
  firstObsRows <- createFirstRows(tempDataSyncS)
  evalRows <- createEvalRows(tempDataSyncS)
  
  df_site <- data.frame(site = unique(tempDataSyncS$featureid))
  df_site$sitef <- seq(1, nrow(df_site), by = 1) # only works for data to fit, not data to predict
  df_huc <- data.frame(huc = as.character(unique(tempDataSyncS$huc)))
  df_huc$hucf <- seq(1, nrow(df_huc), by = 1)
  df_year <- data.frame(year = unique(tempDataSyncS$year))
  df_year$yearf <- seq(1, nrow(df_year), by = 1)
  J <- nrow(df_site)
  M <- nrow(df_huc)
  Ti <- nrow(df_year)
  
  rand_ids <- list(df_site = df_site, df_huc = df_huc, df_year = df_year, J = J, M = M, Ti = Ti)
  
  rand_ids$df_site$site <- as.character(rand_ids$df_site$site)
  rand_ids$df_huc$huc <- as.character(rand_ids$df_huc$huc)
  
  tempDataSyncS <- tempDataSyncS %>%
    dplyr::select(-sitef) %>%
    dplyr::left_join(rand_ids$df_site) %>%
    dplyr::left_join(rand_ids$df_huc) %>%
    dplyr::left_join(rand_ids$df_year)
  
  save(tempDataSync, tempDataSyncS, firstObsRows, evalRows, var.names, df_stds, rand_ids, file = output_file)
}

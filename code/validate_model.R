# Validate model
# requires coef input binary file (coef.list list of dataframes), the covariate_list binary file (cov.list list of character strings) and tempDataSync file
# saves output rmse.table to binary file rmse_table.RData
#
# usage: $ Rscript validate_model.R <input tempDataSync rdata> <input covariate_list rdata> <input coef rdata> <output rmse_table rdata>
# example: $ Rscript validate_model.R ./tempDataSync.RData ./covariate_list.RData ./coef.RData ./rmse_table.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# determine whether to run the code or not
library(jsonlite)
config <- fromJSON('model_config.json')

# get current model run directory
data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1])

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/tempDataSync.RData"), paste0(data_dir, "/covariate_list.RData"), paste0(data_dir, "/coef.RData"), paste0(data_dir, "/rmse_table.RData"), paste0(data_dir, "/valid_results.RData")) # 
}

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
  stop(paste0('Could not find coefficient binary file: ', coef_file))
}
coef.list <- readRDS(coef_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

output_file2 <- args[5]
if (file.exists(output_file2)) {
  warning(paste0('Output file 2 already exists, overwriting: ', output_file2))
}

# ----
if(config[["validate"]]) {
  
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(devtools)
  # install_github("Conte-Ecology/conteStreamTemperature")
  library(conteStreamTemperature)
  library(rjags)
  
  # temporary when not running via bash
  #coef.list <- readRDS("localData/coef.RData")
  #cov.list <- readRDS("localData/covariate-list.RData")
  #load("localData/tempDataSync.RData")
  
  featureid_site <- tempDataSyncS %>%
    dplyr::select(featureid, site) %>%
    dplyr::distinct() %>%
    dplyr::mutate(site = as.character(site))
  
  tempDataSyncS <- predictTemp(data = tempDataSyncS, coef.list = coef.list, cov.list = cov.list, rand_ids = rand_ids) #, featureid_site = featureid_site, validate = FALSE)
  
  tempDataSyncValidS <- predictTemp(data = tempDataSyncValidS, coef.list = coef.list, cov.list = cov.list, rand_ids = rand_ids) #, featureid_site = featureid_site, validate = TRUE)
  
  
  
  #library(ggplot2)
  #ggplot(tempDataSyncValidS, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1,1), colour = 'blue')
  tempDataSyncValidS$resid.r <- tempDataSyncValidS$temp - tempDataSyncValidS$trend
  rmse.valid <- rmse(tempDataSyncValidS$resid.r)
  
  tempDataSyncValidS$resid.r.ar1 <- tempDataSyncValidS$temp - tempDataSyncValidS$tempPredicted
  (rmse.valid.ar1 <- rmse(tempDataSyncValidS$resid.r.ar1))
  
  
  #library(ggplot2)
  #ggplot(tempDataSyncS, aes(temp, tempPredicted)) + geom_point() + geom_abline(aes(1,1), colour = 'blue')
  
  tempDataSyncS$resid.r <- tempDataSyncS$temp - tempDataSyncS$tempPredicted
  
  rmse.fit <- rmse(tempDataSyncS$resid.r)
  rmse.table <- data.frame(rbind(rmse.fit, rmse.valid))
  colnames(rmse.table) <- "rmse"
  
  output_file <- paste0(data_dir, "/rmse_table.RData")
  saveRDS(rmse.table, file=output_file)
  
  foo <- tempDataSyncS %>%
    dplyr::select(featureid, date, trend, tempPredicted, resid.r)%>%
    dplyr::mutate(featureid = as.factor(featureid))
  
  tempDataSync <- tempDataSync %>%
    dplyr::mutate(featureid = as.factor(featureid))
  
  tempDataSync <- tempDataSync %>%
    left_join(foo, by = c("featureid", "date"))
  
  saveRDS(tempDataSync, paste0(data_dir, "/obs_predicted.RData"))
  saveRDS(tempDataSyncValidS, paste0(data_dir, "/valid_results.RData"))
  
}


if(FALSE) {
  #--------------- old stuff to incorporate -------------
  pred <- predictTemp(data = tempDataSyncS, coef.list = coef.list, rand_ids = rand_ids)
  rmse(pred$temp - pred$tempPredicted)
  rmse(pred$temp - pred$trend)
  
  pred_valid <- predictTemp(data = tempDataSyncValidS, coef.list = coef.list, rand_ids = rand_ids)
  rmse(pred_valid$temp - pred_valid$tempPredicted)
  rmse(pred_valid$temp - pred_valid$trend)
  
  data <- tempDataSyncS
  df <- tempDataSyncS
  
  ######## Validation ##########
  
  df <- tempDataSyncValid %>%
    left_join(dplyr::select(pred_valid, featureid, date, fixed.ef, site.ef, huc.ef, year.ef, trend, tempPredicted))
  
  df <- df %>%
    dplyr::mutate(site_day = paste0(df$featureid, "_", df$date))
  site_days <- unique(tempDataSync$site_day)
  sites <- unique(tempDataSync$featureid)
  hucs <- unique(tempDataSync$huc)
  years <- unique(tempDataSync$year)
  
  # overall validation
  
  rmse(df$temp - df$trend)
  rmse(df$temp - df$tempPredicted)
  
  # missing days but sites, hucs, years with data
  valid_miss_days <- df %>%
    dplyr::filter(!(site_day %in% site_days))
  
  rmse(valid_miss_days$temp - valid_miss_days$trend)
  rmse(valid_miss_days$temp - valid_miss_days$tempPredicted)
  
  # missing sites but hucs and years with data
  
  valid_miss_sites <- df %>%
    dplyr::filter(!(featureid %in% sites) & huc %in% hucs & year %in% years)
  
  rmse(valid_miss_sites$temp - valid_miss_sites$trend)
  rmse(valid_miss_sites$temp - valid_miss_sites$tempPredicted)
  
  # sites and huc data but missing years
  
  valid_miss_years <- df %>%
    dplyr::filter(!(year %in% years) & site %in% sites & huc %in% hucs)
  
  rmse(valid_miss_years$temp - valid_miss_years$trend)
  rmse(valid_miss_years$temp - valid_miss_years$tempPredicted)
  
  # no data
  
  valid_miss_all <- df %>%
    dplyr::filter(!(year %in% years) & !(site %in% sites) & !(huc %in% hucs))
  
  rmse(valid_miss_all$temp - valid_miss_all$trend)
  rmse(valid_miss_all$temp - valid_miss_all$tempPredicted)
  
  
  ##################
  g <- ggplot(df, aes(temp, airTemp)) + 
    geom_point() + 
    geom_point(aes(temp, trend), colour = "dark red", alpha = 0.5) + 
    geom_point(aes(temp, tempPredicted), colour = 'blue', alpha = 0.5) + 
    geom_abline(intercept = 0, slope = 1, colour = 'black') + 
    theme_bw() + xlab("observered temperature (C)") + 
    ylab("predicted temperature (C)")
  
  g
  
  #--------- characteristics of sites with good vs. poor predictions ---------
  
  df_poor <- derived.site.metrics %>%
    dplyr::filter(meanRMSE > quantile(derived.site.metrics$meanRMSE, probs = c(0.9), na.rm = TRUE))
  summary(df_poor)
  
  
  
  # can be off by quite a bit (2C) on any given day but see how bad RMSE is for derived metrics like meanJulyTemp
  
  
  # add observed derived metrics to function and maybe airTemp derived metrics - observed metrics only work for years with data so should only be compared to derived metrics for those years - would have to change the functions for this
  # hack
  # valid_obs_pred <- tempDataSyncValid %>%
  #   left_join(dplyr::select(pred_valid, featureid, date, fixed.ef, site.ef, huc.ef, year.ef, trend, tempPredicted)) %>%
  #   dplyr::mutate(temp_obs = temp,
  #                 tempPredicted = trend)
  # 
  # # test
  # all(valid_obs_pred$trend == valid_obs_pred$tempPredicted)
  # 
  # 
  # # doesn't work because don't have 30 days of continuous predictions for the rollapply function unless it's gone through the data prep function. - would have to do by year as well since 
  # obs_metrics_valid <- deriveMetrics(valid_obs_pred)
  # 
  # foo <- deriveMetrics(fullDataSync = fullDataSync)
  # foo <- deriveMetrics(fullDataSync = group_by(df, featureid, year))
  # 
  # bar <- left_join(coef.list$B.site, rand_ids$df_site)
  # dplyr::filter(bar, site == "853196")
  
}

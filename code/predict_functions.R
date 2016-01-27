prepData <- function(catches_string, springFallBPs, df_covariates_upstream, tempDataSync, featureid_lat_lon, featureid_huc8, rand_ids) {
  
  ########## pull daymet records ##########
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname='sheds', host='osensei.cns.umass.edu', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))
  
  qry_daymet <- paste0("SELECT featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe, (tmax + tmin) / 2.0 AS airtemp FROM daymet WHERE featureid IN (", catches_string, ") ;")
  rs <- dbSendQuery(con, statement = qry_daymet)
  climateData <- dbFetch(rs, n=-1)
  
  dbClearResult(rs)
  dbDisconnect(con)
  #dbUnloadDriver(drv)
  
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
                 "swe",
                 "impoundArea")
  
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
  
  return(list(fullDataSync = fullDataSync, fullDataSyncS = fullDataSyncS))
}









predictTemp <- function(fullDataSyncS, coef.list, rand_ids, Random_AR1 = FALSE) {
  
  
  if(!exists("fullDataSyncS$sitef")) {
    fullDataSyncS <- fullDataSyncS %>%
      left_join(rand_ids$df_site)
  }
  if(!exists("fullDataSyncS$hucf")) {
    fullDataSyncS <- fullDataSyncS %>%
      left_join(rand_ids$df_huc)
  }
  if(!exists("fullDataSyncS$yearf")) {
    fullDataSyncS <- fullDataSyncS %>%
      left_join(rand_ids$df_year)
  }
  
  ############# Predictions ##############
  #fullDataSyncS <- predictTemp(data = fullDataSyncS, coef.list = coef.list, cov.list = cov.list, featureid_site = featureid_site)
  
  fixed.ef <- as.numeric(coef.list$B.fixed$mean) # separate out the iteration or do for mean/median
  
  # add specific random effects to the dataframe
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.site)
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.huc) # problem with validation data, need to use the mean when huc don't match
  fullDataSyncS <- left_join(fullDataSyncS, coef.list$B.year)
  
  
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
  
  if(Random_AR1) {
    #B.ar1.sub <- data.frame(sitef = rand_ids$df_site$sitef)
    B.ar1.sub <- coef.list$B.ar1 %>%
      dplyr::select(sitef, mean) %>%
      dplyr::rename(B.ar1 = mean)
    fullDataSyncS <- left_join(fullDataSyncS, B.ar1.sub, by = c("sitef"))
    fullDataSyncS <- fullDataSyncS %>%
      dplyr::mutate(B.ar1 = ifelse(is.na(B.ar1), mean(B.ar1.sub$B.ar1, na.rm = T), B.ar1)) %>%
      dplyr::arrange(featureid, date) 
  } else {
    fullDataSyncS$B.ar1 <- coef.list$B.ar1$mean
    fullDataSyncS <- fullDataSyncS %>% dplyr::arrange(featureid, date) 
  }

  
  fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$tempPredicted <- fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$trend + fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$B.ar1 * fullDataSyncS[which(!is.na(fullDataSyncS$prev.err)), ]$prev.err
  
  # unofficial warning message
#   mean.pred <- mean(fullDataSync$tempPredicted, na.rm = T)
#   if(mean.pred == "NaN") {
#     warning(paste0(i, " of ", n.loops, " loops has no predicted temperatures"))
#   } 
  
  fullDataSyncS <- dplyr::arrange(fullDataSyncS, rowNum)
  
  fullDataSyncS <- data.frame(unclass(fullDataSyncS), stringsAsFactors = FALSE)
  
  return(fullDataSyncS)
  
}

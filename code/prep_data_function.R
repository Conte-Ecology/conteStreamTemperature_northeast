#' @title prepData: Wrapper to prepare data for analysis or predictions
#'
#' @description
#' \code{prepData} Wrapper to prepare data for analysis or predictions
#'
#' @param catches_string Vector of catchment featureid to prepare for analysis
#' @param springFallBPs Dataframe of spring-fall breakpoints
#' @param df_covariates_upstream Dataframe of covariates
#' @param tempDataSync Data used for modeling
#' @param featureid_lat_lon Dataframe linking featureid to lat and lon
#' @param featureid_huc8 Dataframe linking featureid to hucs
#' @param rand_ids Dataframe of random site, huc, and year info
#' @param df_stds Dataframe of means and standard deviations used for standardizing continuous covariates
#' @details
#' This function is a mess but currently works. Lots of redundancy, especially in the data imported.
#' var: blah, blah, blah
#' value: something, something
#' @export
prepData <- function (catches_string, springFallBPs, df_covariates_upstream, tempDataSync, featureid_lat_lon, featureid_huc8, rand_ids, df_stds) {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "sheds", host = "osensei.cns.umass.edu",
      user = options("SHEDS_USERNAME"), password = options("SHEDS_PASSWORD"))
  qry_daymet <- paste0("SELECT featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe, (tmax + tmin) / 2.0 AS airtemp FROM daymet WHERE featureid IN (",
    catches_string, ") ;")
  rs <- dbSendQuery(con, statement = qry_daymet)
  climateData <- dbFetch(rs, n = -1)
  dbClearResult(rs)
  dbDisconnect(con)
  
  mean.spring.bp <- mean(dplyr::filter(springFallBPs, finalSpringBP !=
                                         "Inf")$finalSpringBP, na.rm = T)
  mean.fall.bp <- mean(dplyr::filter(springFallBPs, finalFallBP !=
                                       "Inf")$finalFallBP, na.rm = T)
  foo <- springFallBPs %>% 
    dplyr::mutate(site = as.character(site),
                  featureid = as.integer(site),
                  finalSpringBP = ifelse(finalSpringBP == "Inf" | is.na(finalSpringBP), mean.spring.bp, finalSpringBP),
                  finalFallBP = ifelse(
                    finalFallBP == "Inf" | is.na(finalFallBP),
                    mean.fall.bp, finalFallBP))
  
  fullDataSync <- climateData %>% 
    left_join(df_covariates_upstream) %>% 
    left_join(dplyr::select(tempDataSync,date, featureid, site, temp)) %>%
    left_join(featureid_huc8) %>% 
    left_join(featureid_lat_lon) %>% 
    dplyr::mutate(year = as.numeric(format(date,"%Y")), 
                  airTemp = (tmax + tmin) / 2) %>% 
    left_join(dplyr::select(foo,-site)) %>% 
    dplyr::mutate(finalSpringBP = ifelse(finalSpringBP == "Inf" | is.na(finalSpringBP), mean.spring.bp, finalSpringBP),
                  finalFallBP = ifelse(finalFallBP == "Inf" | is.na(finalFallBP), mean.fall.bp, finalFallBP)) %>% 
    dplyr::mutate(dOY = yday(date)) %>%
    dplyr::filter(AreaSqKM < 200)
  
  rm(climateData)
  gc()
  
  fullDataSync <- fullDataSync[order(fullDataSync$featureid,
                                     fullDataSync$year, fullDataSync$dOY),]
  
  fullDataSync <- fullDataSync %>% 
    group_by(featureid, year) %>%
    arrange(featureid, year, dOY) %>% 
    mutate(
      impoundArea = AreaSqKM * allonnet, 
      airTempLagged1 = lag(airTemp, n = 1, fill = NA),
      temp5p = rollapply(data = airTempLagged1, width = 5,
                         FUN = mean, align = "right", fill = NA, na.rm = T),
      temp7p = rollapply(data = airTempLagged1, width = 7,
                         FUN = mean, align = "right", fill = NA, na.rm = T),
      prcp2 = rollsum(x = prcp, 2, align = "right", fill = NA),
      prcp7 = rollsum(x = prcp, 7, align = "right", fill = NA),
      prcp30 = rollsum(x = prcp, 30, align = "right", fill = NA)
    )
  
  fullDataSync <- fullDataSync %>% 
    dplyr::filter(
      dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>% 
    dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp)
  
  var.names <- c(
    "airTemp", "temp7p", "prcp", "prcp2", "prcp7",
    "prcp30", "dOY", "forest", "herbaceous", "agriculture",
    "devel_hi", "developed", "AreaSqKM", "allonnet", "alloffnet",
    "surfcoarse", "srad", "dayl", "swe", "impoundArea"
  )
  
  fullDataSync <- fullDataSync %>% 
    mutate(
      HUC8 = as.character(HUC8),
      huc8 = as.character(HUC8), 
      huc = as.character(HUC12),
      site = as.numeric(as.factor(featureid))
    )
  
  fullDataSyncS <- stdCovs(x = fullDataSync, y = df_stds, var.names = var.names)
  
  fullDataSyncS <- addInteractions(fullDataSyncS)
  
  fullDataSyncS <- dplyr::arrange(fullDataSyncS, featureid,
                                  date)
  fullDataSyncS$site <- as.character(fullDataSyncS$featureid)
  if (exists("fullDataSyncS$sitef")) {
    fullDataSyncS <- dplyr::select(fullDataSyncS,-sitef)
  }
  fullDataSyncS <-
    fullDataSyncS %>% left_join(rand_ids$df_site) %>%
    left_join(rand_ids$df_huc) %>% left_join(rand_ids$df_year)
  fullDataSyncS <- indexDeployments(fullDataSyncS, regional = TRUE)
  
  return(list(fullDataSync = fullDataSync, fullDataSyncS = fullDataSyncS))
}

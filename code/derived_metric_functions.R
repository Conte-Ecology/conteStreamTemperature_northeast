deriveMetrics <- function(data) {
  byfeatureid <- group_by(data, featureid)
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

# Mean July Air Temp
meanJulyAir <- byfeatureidYear %>%
  dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
  dplyr::filter(month == 7) %>%
  dplyr::summarise(JulyTemp = mean(airTemp, na.rm = T)) %>%
  dplyr::summarise(meanJulyAir = mean(JulyTemp))
derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanJulyAir, by = "featureid")
rm(meanJulyAir)

# Mean July Summer Temp
meanJulyTemp <- byfeatureidYear %>%
  dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
  dplyr::filter(month == 7) %>%
  dplyr::summarise(JulyTemp = mean(tempPredicted, na.rm = T)) %>%
  dplyr::summarise(meanJulyTemp = mean(JulyTemp))
derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanJulyTemp, by = "featureid")
rm(meanJulyTemp)

# Mean July Obs Temp
meanJulyObs <- byfeatureidYear %>%
  dplyr::mutate(month = as.numeric(format(date, "%m"))) %>%
  dplyr::filter(month == 7)

nJulyObs <- meanJulyObs %>%
  dplyr::filter(!is.na(temp)) %>%
  dplyr::summarise(nJulyObs = n())

nJulyObsYears <- nJulyObs %>%
  dplyr::filter(nJulyObs >= 30) %>% # at least 25 days with observed data
  dplyr::summarise(nYearsJulyObs = n())

if(nrow(nJulyObsYears) == 0) {
  meanJulyObs <- data.frame(featureid = unique(meanJulyObs$featureid),
                            meanJulyObs = NA,
                            nYearsJulyObs = 0)
} else {
  meanJulyObs <- nJulyObs %>%
    left_join(meanJulyObs) %>%
    dplyr::filter(nJulyObs >= 30) %>%
    summarise(meanJulyObs = mean(temp, na.rm = T)) %>%
    left_join(nJulyObsYears)
}

derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, meanJulyObs, by = "featureid")
rm(meanJulyObs)

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
  dplyr::arrange(featureid, year, dOY) %>%
  dplyr::mutate(mm30Day = rollapply(data = tempPredicted, 
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
  dplyr::mutate(freqMaxTemp = yearsMaxTemp / length(unique(byfeatureidYear$year)))
derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "yearsMaxTemp"] <- NA
derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "freqMaxTemp"] <- NA

derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
  dplyr::rename(yearsMaxTemp.22 = yearsMaxTemp,
         freqMaxTemp.22 = freqMaxTemp)
  
derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
  dplyr::mutate(yearsMaxTemp.22 = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.22), 0, yearsMaxTemp.22),
                freqMaxTemp.22 = ifelse(!is.na(meanMaxTemp) & is.na(freqMaxTemp.22), 0, freqMaxTemp.22))

derivedfeatureidMetrics <- calcYearsMaxTemp(grouped.df = byfeatureidYear, derived.df = derivedfeatureidMetrics, temp.threshold = 23.5) %>%
  mutate(yearsMaxTemp = ifelse(is.na(yearsMaxTemp), 0, as.numeric(yearsMaxTemp))) %>%
  dplyr::rename(yearsMaxTemp.23.5 = yearsMaxTemp) %>%
  dplyr::mutate(freqMaxTemp.23.5 = yearsMaxTemp.23.5 / length(unique(byfeatureidYear$year)))
derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "yearsMaxTemp.23.5"] <- NA
derivedfeatureidMetrics[which(is.na(derivedfeatureidMetrics$meanMaxTemp)), "freqMaxTemp.23.5"] <- NA
derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
  dplyr::mutate(yearsAcute.MADEP = ifelse(!is.na(meanMaxTemp) & is.na(yearsMaxTemp.23.5), 0, yearsMaxTemp.23.5),
                freqAcute.MADEP = ifelse(!is.na(meanMaxTemp) & is.na(freqMaxTemp.23.5), 0, freqMaxTemp.23.5))

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
  error_metrics <- bar %>%
    dplyr::summarise(RMSE = rmse(error),
                     MAE = mae(error),
                     NSE = nse(temp, tempPredicted)) %>%
    dplyr::summarise(meanRMSE = mean(RMSE, na.rm = T),
                     meanMAE = mean(MAE, na.rm = T),
                     meanNSE = mean(NSE, na.rm = T))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, dplyr::select(error_metrics, featureid, meanRMSE, meanMAE, meanNSE), by = "featureid")
} else {
  derivedfeatureidMetrics$meanRMSE <- NA_real_
  derivedfeatureidMetrics$meanMAE <- NA_real_
  derivedfeatureidMetrics$meanNSE <- NA_real_
  #     derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
  #       dplyr::mutate(meanRMSE = NA) %>%
  #       dplyr::mutate(meanRMSE = as.numeric(meanRMSE))
}
rm(bar)

############# ADD RMSE by Trend in addition to predicted with AR1 #########

# RMSE for each featureid (flag highest)
bar <- byfeatureidYear %>%
  filter(!(is.na(temp) & !is.na(trend))) %>%
  mutate(error = temp - trend)

if(dim(bar)[1] > 0) {
  error_metrics <- bar %>%
    dplyr::summarise(RMSE_trend = rmse(error),
                     MAE_trend = mae(error),
                     NSE_trend = nse(temp, tempPredicted)) %>%
    dplyr::summarise(meanRMSE_trend = mean(RMSE_trend, na.rm = T),
                     meanMAE_trend = mean(MAE_trend, na.rm = T),
                     meanNSE_trend = mean(NSE_trend, na.rm = T))
  derivedfeatureidMetrics <- left_join(derivedfeatureidMetrics, dplyr::select(error_metrics, featureid, meanRMSE_trend, meanMAE_trend, meanNSE_trend), by = "featureid")
} else {
  derivedfeatureidMetrics$meanRMSE_trend <- NA_real_
  derivedfeatureidMetrics$meanMAE_trend <- NA_real_
  derivedfeatureidMetrics$meanNSE_trend <- NA_real_
  #     derivedfeatureidMetrics <- derivedfeatureidMetrics %>%
  #       dplyr::mutate(meanRMSE = NA) %>%
  #       dplyr::mutate(meanRMSE = as.numeric(meanRMSE))
}
rm(bar)

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

return(metrics)
}


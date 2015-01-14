library(ggplot2)
library(dplyr)
library(DataCombine) # for the slide function
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

load((paste0(dataOutDir, 'modSummary.RData')))
load(paste0(dataLocalDir, 'daymetFullRecordObservedMASites.RData'))
load(paste0(dataOutDir, 'tempDataSync.RData'))

load(paste0(dataOutDir, 'springFallBreakpoints.RData'))


#Northeast
CTDEP  <- F
MAFW   <- T
MAUSGS <- T
MADEP  <- T 
NHFG   <- F
NHDES  <- F
USFS   <- F
VTFWS  <- F
MEDMR  <- F

#Montana
MTUSGSYellowstone <- F
MTUSGSGlacier <- F

sourceChoice <- list( CTDEP,   MAFW,   MAUSGS, MADEP,   NHFG,   NHDES,   MEDMR,   USFS,   VTFWS,    MTUSGSYellowstone,   MTUSGSGlacier)
sourceNames  <- c   ('CTDEP', 'MAFW', 'MAUSGS', 'MADEP', 'NHFG', 'NHDES', 'MEDMR', 'USFS', 'VTFWS',  'MTUSGSYellowstone', 'MTUSGSGlacier')

dataSource <- sourceNames[sourceChoice == T]

fields <- c("agency", "date", "AgencyID", "year", "site", "date", "dOY", "temp", "airTemp", "prcp", "srad", "dayl", "swe")


var.names <- c("Latitude", "Longitude", "airTemp", "airTempLagged1", "airTempLagged2", "prcp", "prcpLagged1", "prcpLagged2", "prcpLagged3", "dOY", "Forest", "Herbacious", "Agriculture", "Developed", "TotDASqKM", "ReachElevationM", "ImpoundmentsAllSqKM", "HydrologicGroupAB", "SurficialCoarseC", "CONUSWetland", "ReachSlopePCNT", "srad", "dayl", "swe")

prepDataWrapper(var.names = var.names, predict.daymet = TRUE, dataInDir = dataInDir, dataOutDir = dataOutDir, file = paste0(dataOutDir, "tempDataSync-daymet.Rdata"), validate = FALSE) # maybe consider having this return rather than save, otherwise have to load here

load(paste0(dataOutDir, "tempDataSync-daymet.Rdata"))

tempDataSyncS$tempPredicted <- NA
tempDataSyncS$tempPredicted <- predictTemp(data = tempDataSyncS, coef.list = coef.list, cov.list = cov.list, firstObsRows = firstObsRows, evalRows = evalRows)




# plots in functions
plotPredict(observed = tempDataSync, predicted = tempDataSyncS, siteList = "ALL", yearList = "ALL", dir = paste0(dataLocalDir, 'plots/fullRecord/'))


###Derived metrics

# Mean maximum daily mean temperature by site (over years)
bySite <- group_by(tempFull, site)
bySiteYear <- group_by(bySite, year, add = TRUE)
maxTemp <- filter(bySite, tempPredicted == max(tempPredicted))
maxTempSite <- summarise(maxTemp, mean(tempPredicted)) # not needed - already max.t
#summarise(by.site.year, sd(mean(tempPredicted))) # not working based on filter or grouping

(maxTempSiteYear <- summarise(bySiteYear, max(tempPredicted)))
names(maxTempSiteYear) <- c("site", "year", "maxTempPredicted")
derivedSiteMetrics <- summarise(maxTempSiteYear, meanMaxTemp = mean(maxTempPredicted))
# maxTempSiteYear1 <- left_join(as.data.frame(maxTempSiteYear), tempFull, by=c("site", "tempPredicted"))

# Maximum max daily mean temperature
maxMaxTemp <- bySiteYear %>%
  summarise(maxTemp = max(tempPredicted)) %>%
  summarise(maxMaxTemp = max(maxTemp))

derivedSiteMetrics <- left_join(derivedSiteMetrics, maxMaxTemp, by = "site")

# ggplot(tempFull, aes(dOY, temp)) + geom_point(size=1, colour='black') + geom_point(aes(dOY, tempPredicted), colour = 'red', size=0.75) + ylab(label="Stream temperature (C)") + xlab("Day of the year") + geom_point(data=maxTempSiteYear1, aes(dOY, tempPredicted), colour = "green") + facet_grid(site ~ year) # max temp points all replicated on every panel

# Number of days with stream temp > 18C
meanDays18 <- bySiteYear %>%
  filter(tempPredicted > 18) %>%
  summarise(days18 = n()) %>%
  summarise(meanDays18 = mean(days18))

derivedSiteMetrics <- left_join(derivedSiteMetrics, meanDays18, by = "site")

# Number of years with mean maximum over 18 C
yearsMaxTemp18 <- summarise(
  filter(summarise(bySiteYear, maxTemp = max(tempPredicted)), maxTemp > 18),
  yearsMaxTemp18 = n()
)
derivedSiteMetrics <- left_join(derivedSiteMetrics, yearsMaxTemp18, by = "site")

# frequency of years with a mean max over 18 C
derivedSiteMetrics <- mutate(derivedSiteMetrics, freqMax18 = yearsMaxTemp18/length(unique(bySiteYear$year)))

# Resistance to peak air temperature
## Need to think of a way to make more general rather than by specific dOY (60 day max moving window air temp?)
meanResist <- bySiteYear %>%
  filter(dOY >= 145 & dOY <= 275) %>%
  mutate(absResid = abs(airTemp - tempPredicted)) %>%
  summarise(resistance = sum(absResid)) %>%
  summarise(meanResist = mean(resistance))

derivedSiteMetrics <- left_join(derivedSiteMetrics, meanResist, by = "site")


WB.2011.summer <- tempFull[which(tempFull$site == "MAUSGS_WEST_BROOK" & tempFull$year == 2011 & tempFull$dOY >=145 & tempFull$dOY <= 275), ]
sum(WB.2011.summer$airTemp - WB.2011.summer$tempPredicted)

ggplot(tempFull[which(tempFull$site == "MAUSGS_WEST_BROOK" & tempFull$year == 2011), ], aes(dOY, tempPredicted)) + 
  geom_point(size=2, colour = "red") + geom_line(colour = 'red') +
  geom_point(data=tempFull[which(tempFull$site == "MAUSGS_WEST_BROOK" & tempFull$year == 2011), ], aes(dOY, airTemp), colour = "black", size=2) + 
  geom_line(data=tempFull[which(tempFull$site == "MAUSGS_WEST_BROOK" & tempFull$year == 2011), ], aes(dOY, airTemp), colour = "black") + 
  geom_ribbon(data = tempFull[which(tempFull$site == "MAUSGS_WEST_BROOK" & tempFull$year == 2011 & tempFull$dOY >=145 & tempFull$dOY <= 275), ], aes(x=dOY, ymin=tempPredicted, ymax=airTemp), fill="dark grey", alpha=.5) +
  xlab("Day of the year") +
  ylab("Temperature (C)") #+ theme_classic()

ggplot(tempFull[which(tempFull$site == "WB OBEAR" & tempFull$year == 2010), ], aes(dOY.real, tempPredicted)) + 
  geom_point(size=2, colour = "black") + geom_line(colour = 'black') +
  geom_abline(intercept = 18, slope=0, colour='red') +
  geom_point(data = tempFull[which(tempFull$site == "WB OBEAR" & tempFull$year == 2010 & tempFull$tempPredicted >= 18), ], aes(dOY.real, tempPredicted), colour='red') +
  xlab("Day of the year") +
  ylab("Stream temperature (C)") #+ theme_classic()

# Reset ggplot2 theme default to gray
theme_set(theme_gray())



# Air-Water Resiliency

# RMSE for each site (flag highest)
meanRMSE <- bySiteYear %>%
  filter(!(is.na(temp))) %>%
  mutate(error2 = (temp - tempPredicted)^2) %>%
  summarise(RMSE = sqrt(mean(error2))) %>%
  summarise(meanRMSE = mean(RMSE))

derivedSiteMetrics <- left_join(derivedSiteMetrics, meanRMSE, by = "site")

# total observations (days with data) per site
totObs <- bySiteYear %>%
  filter(!is.na(temp)) %>%
  summarise(Obs = n()) %>%
  summarise(totObs = sum(Obs))

derivedSiteMetrics <- left_join(derivedSiteMetrics, totObs, by = "site")

# Flag based on RMSE > 90%
derivedSiteMetrics <- mutate(derivedSiteMetrics, flag = ifelse(meanRMSE > quantile(derivedSiteMetrics$meanRMSE, probs = c(0.9), na.rm=TRUE), "Flag", ""))

summary(derivedSiteMetrics)

derivedSiteMetricsClean <- na.omit(derivedSiteMetrics)
write.table(derivedSiteMetricsClean, file = 'reports/MADEP/derivedSiteMetrics.csv', sep=',', row.names = F)


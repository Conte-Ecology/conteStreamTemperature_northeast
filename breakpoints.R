# Compute spring/fall breakpoints
# requires masterData, covariateData input binary files
# saves output springFallBPs to binary file
#
# usage: $ Rscript breakpoints.R <input temperatureData rdata> <input climateData rdata> <input covariateData rdata> <output springFallBPs rdata>
# example: $ Rscript breakpoints.R ./temperatureData.RData ./climateData.RData ./covariateData.RData ./springFallBPs.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

library(devtools)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(conteStreamTemperature)

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
temperatureData_file <- args[1]
if (!file.exists(masterData_file)) {
  stop(paste0('Could not find temperatureData binary file: ', temperatureData_file))
}
temperatureData <- readRDS(temperatureData_file)

climateData_file <- args[2]
if (!file.exists(climateData_file)) {
  stop(paste0('Could not find climateData binary file: ', climateData_file))
}
climateData <- readRDS(climateData_file)

covariateData_file <- args[3]
if (!file.exists(covariateData_file)) {
  stop(paste0('Could not find covariateData binary file: ', covariateData_file))
}
covariateData <- readRDS(covariateData_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

# combine into masterData file
masterData <- climateData %>%
  left_join(temperatureData, by = c())

# add dOY and year columns
masterData <- mutate(masterData,
                     dOY=yday(date),
                     year=year(date))

# # Select "T" or "F" for using agency data
# sources <- list (
#   # Northeast
#   CTDEP  = CTDEP  <- T,
#   MADEP  = MADEP  <- T,
#   MAFW   = MAFW   <- T,
#   MAUSGS = MAUSGS <- T,
#   MEDMR  = MEDMR  <- T,
#   MEFWS  = MEFWS  <- T,
#   NHDES  = NHDES  <- T,
#   NHFG   = NHFG   <- T,
#   USFS   = USFS   <- T,
#   VTFWS  = VTFWS  <- T,
#   
#   # Montana
#   MTUSGSYellowstone = MTUSGSYellowstone <- F,
#   MTUSGSGlacier     = MTUSGSGlacier     <- F
# )
# 
# dataSources <- names(sources[(sources == T)])
# 
# Enter the common fields from the temperature ("site" must be one).
tempFields <- c('site', 'year', 'dOY', 'date', 'agency', 'temp', 'airTemp')

# Enter the specific covariate fields you want to pull ("site" must be one) or for the entire file, enter "ALL"
covFields <- c('site', 'HUC4', 'HUC8', 'HUC12')

# Read in data records and join into one dataframe
# ------------------------------------------------
# e <- readStreamTempData(timeSeries = T, covariates = T, dataSourceList = dataSources, fieldListTS = tempFields, fieldListCD = covFields, directory = dataInDir )

# select temperature data columns
masterData <- masterData[, tempFields]

# select covariate data columns
covariateData <- covariateData[, covFields]

# merge masterData and covariateData
e <- left_join(masterData, covariateData)

## Calculate the temperature index which is the key metric for estimating the synchrony between air and water temperature.

# Calculate temperature index. Add small # to avoid infinity
e$tempIndex <- (e$temp-e$airTemp)/(e$temp + 0.00000001)

# Define list of sites
siteList <- unique(e$site)

# Order by group and date
e <- e[order(e$site,e$year,e$dOY),]

# For checking the order of e
e$count <- 1:length(e$year)

# Define the site/year ID
e$siteYear <- paste(e$site,e$year,sep='_')

# Maintain order
e <- e[order(e$count),]

## Get the moving mean of the temp index for each site and put into the data frame

# Set frame sizefor moving mean, which is centered by default
window <- 10

# Number of sites
nSites <- length(siteList)

# Unique site and year combos 
siteYearCombos <- unique(e[,c('site','year')])

# Add columns for moving mean and sd
e$movingMean <- NA

# Loop through site/year combinations calculating moving means
for (i in 1:nrow(siteYearCombos)){
  
  #library(zoo)
  
  # Status
  #print(c(i,as.character(siteYearCombos$site[i]),siteYearCombos$year[i],i/nrow(siteYearCombos)))
  
  # Index current site/year
  currSite <- which(e$site == as.character(siteYearCombos$site[i]) & e$year == siteYearCombos$year[i] )
  
  # Only calculate for sites with enough data
  if(length(currSite) >= window){currMean <-  rollapply(e$tempIndex[currSite], width=window, fill=NA, mean)} else(currMean <- NA)
  
  # Add to main dataframe
  e$movingMean[currSite] <- currMean
}

# Maintain order
e <- e[order(e$count),]

# Define breakpoint time period and range for tempIndex
beginningDayForCI <- 125
endingDayForCI <- 275
loCI <- 0.001
hiCI <- 0.999

for ( i in 1:nrow(siteYearCombos)){
  
  # Print status
  print(i)
  
  # Index sites, years, and HUCs
  tempBreaks <- data.frame( year  = as.numeric  (siteYearCombos$year[i]),
                            site  = as.character(siteYearCombos$site[i]),
                            HUC12 = as.character(unique(e$HUC12[which(e$site == siteYearCombos$site[i])])),
                            HUC8  = as.character(unique(e$HUC8 [which(e$site == siteYearCombos$site[i])])),
                            HUC4  = as.character(unique(e$HUC4 [which(e$site == siteYearCombos$site[i])])),
                            quantileLo = NA,
                            quantileHi = NA)
  
  # Calculate the tempindex quantiles
  tmp <- e[e$site == siteYearCombos$site[i] & e$year  %in% siteYearCombos$year[i] & e$dOY %in% beginningDayForCI:endingDayForCI,'tempIndex']
  if (any(!is.na(tmp))){
    TIQ <- quantile(tmp, probs=c(loCI,0.5,hiCI),na.rm=T)
    
    # High and low quantiles
    tempBreaks$quantileLo <- TIQ[1]
    tempBreaks$quantileHi <- TIQ[3]
  }
  
  # Add current site to "breaks"
  if ( i == 1 ) { breaks <- tempBreaks } else( breaks <- rbind(breaks, tempBreaks))
  
} 

# Add columns used later
breaks$springBPComplete <- FALSE
breaks$fallBPComplete <- FALSE
breaks$springOrFallBPComplete <- FALSE
breaks$springBP <- NA
breaks$fallBP   <- NA

## Use runs analysis of the movingMean to define spring and fall breakpoints:
  
# Set range (dOY) and count for assigning spring BP
minCompleteDOYBP1 <- 15
maxCompleteDOYBP1 <- 175
numForCompleteBP1 <- round( ( maxCompleteDOYBP1-minCompleteDOYBP1 ) * 0.9 )

# Set range (dOY) and count for assigning fall BP
minCompleteDOYBP3 <- 225
maxCompleteDOYBP3 <- 350
numForCompleteBP3 <- round( ( maxCompleteDOYBP3-minCompleteDOYBP3 ) * 0.9 )

# Number of days in a row that need to be within the CIs to get assigned synchronised (referred to as numForward range)
numForwardSpring <- 10
numForwardFall   <- 16

# Loop through all sites
for (j in 1:nSites){
  
  #library(plyr)
  
  # Index current site
  # ------------------
  e1 <- e[e$site == siteList[j],]
  
  # Index spring range
  # ------------------
  e3Spring <- e1[ e1$dOY >= minCompleteDOYBP1 & e1$dOY <= maxCompleteDOYBP1, ]
  
  # Empty out from previous run
  completeYearsSpring <- NULL 
  
  # If statement to avoid error if e3Spring is empty
  if ( !empty( e3Spring ) ) {  
    
    # Determine which years have complete records in spring
    completeSpring <- as.data.frame( table( e3Spring$year,is.na( e3Spring$temp ) ) )
    incompleteYearsSpring <- as.numeric(as.character(completeSpring$Var1[completeSpring$Var2 == 'FALSE' & completeSpring$Freq <  numForCompleteBP1]))
    completeYearsSpring <-   as.numeric(as.character(completeSpring$Var1[completeSpring$Var2 == 'FALSE' & completeSpring$Freq >= numForCompleteBP1]))
  }
  
  # Index fall range
  # ----------------
  e3Fall <- e1[ e1$dOY >= minCompleteDOYBP3 & e1$dOY <= maxCompleteDOYBP3, ]
  
  # Empty out from previous run	
  completeYearsFall <- NULL
  
  # If statement to avoid error if e3Fall is empty
  if ( !empty( e3Fall ) ) {
    
    # Determine which years have complete records in fall
    completeFall <- as.data.frame( table( e3Fall$year,is.na( e3Fall$temp ) ) )
    incompleteYearsFall <- as.numeric(as.character(completeFall$Var1[completeFall$Var2 == 'FALSE' & completeFall$Freq <  numForCompleteBP3]))
    completeYearsFall <-   as.numeric(as.character(completeFall$Var1[completeFall$Var2 == 'FALSE' & completeFall$Freq >= numForCompleteBP3]))
  } 
  
  # Years with either a complete spring or complete fall record
  completeYearsSpringOrFall <- unique(c(completeYearsSpring,completeYearsFall))
  
  # Loop through the years with at least one complete season
  for (year in completeYearsSpringOrFall){ 
    
    # Print status
    print(c('BP 1 and 3',j,as.character(siteList[j]),year))
    
    # New column for selecting years with at least one complete season
    breaks$springOrFallBPComplete[ breaks$year == year & breaks$site == siteList[j] ] <- TRUE
    
    # Index the high and low quantiles calculated from the tempIndex
    lo <- breaks$quantileLo[breaks$year == year & breaks$site == siteList[j]] 
    hi <- breaks$quantileHi[breaks$year == year & breaks$site == siteList[j]] 
    
    # Index current year
    eYear <- e1[e1$year == year, ] 
    
    # Spring Breakpoint Calculation
    # -----------------------------
    
    # Create dataframe for calculating number of synchronized days in a row. 
    runsSpring <- data.frame(array(NA,c(1,numForwardSpring)))
    
    # Only calculate if it is a complete season
    if(year %in% completeYearsSpring){
      
      # Loop through approximate time forward until breakpoint in ascending water temp
      for (i in 1:(200)){
        
        # From the current day, loop forward through the numForward range to determined which days are in sync
        for (ii in 2:numForwardSpring ){
          
          # A 1 gets assigned if the moving mean of that day is within the CI range or 
          #     if the iteration falls out of the approximated range examined. If the moving
          #     mean is outside of the range, it gets assigned a zero.
          if( (i+ii-2) %in% eYear$dOY ) {
            runsSpring[ i,ii ] <- 1*((eYear$movingMean[ eYear$dOY == (i+ii-2) ] >= lo) & (eYear$movingMean[ eYear$dOY == (i+ii-2) ] <= hi))
          } else (runsSpring[ i,ii ] <- 1  )
          
        }# end numForward loop
        
        # Determine if all of the days in the numForward range are in sync. If all days within numForward
        #   are in sync (assigned a 1), the product will be a 1, otherwise it is NA.
        runsSpring[ i,1 ] <- prod( runsSpring[ i, 2:numForwardSpring ] )
        
      }# End approximated seasonal loop
      
      # The first day where all of the days ahead of it are in sync (in the numForward range) will be the minimum day with a 1.
      #   This day gets assigned the spring breakpoint
      breaks$springBP[ breaks$year == year & breaks$site == siteList[j] ] <- min(which(runsSpring[,1] == 1))
      
      # Fill in the complete springBP column
      breaks$springBPComplete[ breaks$year == year & breaks$site == siteList[j] ] <- TRUE
    } #completeYearsSpring if statement
    
    
    # Fall Breakpoint Calculation
    # ---------------------------
    
    # Create dataframe for calculating number of days in a row within range
    runsFall   <- data.frame(array(NA,c(1,numForwardFall)))
    
    # Only calculate if it is a complete season
    if(year %in% completeYearsFall){
      
      # Determine the point to stop to keep from going past lower limit if dOY
      stopLoop <- max( c( minCompleteDOYBP3,min(eYear$dOY)+numForwardFall + 1 ) )  
      
      # Loop through the approximate time backward until descending water temp
      for (i in  max(eYear$dOY):stopLoop){
        
        # From the current day, loop backward through the numForward range to determined which days are in sync
        for (ii in 2:numForwardFall ){
          
          # A 1 gets assigned if the moving mean of that day is within the CI range or 
          #     if the iteration falls out of the approximated range examined. If the moving
          #     mean is outside of the range, it gets assigned a zero.
          if( (i-ii+2) %in% eYear$dOY ) { 
            runsFall[ i,ii ] <- 1*((eYear$movingMean[ eYear$dOY == (i-ii+2) ] >= lo) & (eYear$movingMean[ eYear$dOY == (i-ii+2) ] <= hi))
          } else(runsFall[ i,ii ] <- 1 )
          
        }# end numForward loop
        
        # Determine if all of the days in the numForward range are in sync. If all days within numForward
        #   are in sync (assigned a 1), the product will be a 1, otherwise it is NA.
        runsFall[ i,1 ] <- prod( runsFall[ i, 2:numForwardFall ] )
        
      }# End approximated seasonal loop
      
      # The last day where all of the days ahead of it are in sync (in the numForward range) will be the minimum day with a 1.
      #   This day gets assigned the fall breakpoint
      breaks$fallBP[ breaks$year == year & breaks$site == siteList[j] ] <- max(which(runsFall[,1] == 1))
      
      # Fill in the complete fallBP column
      breaks$fallBPComplete[ breaks$year == year & breaks$site == siteList[j] ] <- TRUE
      
    }	#completeYearsFall if statement
    
  } #completeYearsSpringOrFall loop
  
} #site loop


### This section determines the mean breakpoint from the smallest scale where a mean exists and assigns it to sites that did not have enough data to calculate a breakpoint (start with site mean and work up to HUC4 mean).

# Calculate mean BPs across different scales
meanBPSite  <- ddply( breaks, .(site) , summarise, meanSpringBPSite  = mean(springBP,na.rm=T), meanFallBPSite  = mean(fallBP,na.rm=T) )
meanBPHUC12 <- ddply( breaks, .(HUC12), summarise, meanSpringBPHUC12 = mean(springBP,na.rm=T), meanFallBPHUC12 = mean(fallBP,na.rm=T) )
meanBPHUC8  <- ddply( breaks, .(HUC8) , summarise, meanSpringBPHUC8  = mean(springBP,na.rm=T), meanFallBPHUC8  = mean(fallBP,na.rm=T) )
meanBPHUC4  <- ddply( breaks, .(HUC4) , summarise, meanSpringBPHUC4  = mean(springBP,na.rm=T), meanFallBPHUC4  = mean(fallBP,na.rm=T) )

# Merge in mean BPs to "breaks"
breaks <- merge( x = breaks, y = meanBPSite , by = 'site' , all.x = T, all.y = F, sort = F)
breaks <- merge( x = breaks, y = meanBPHUC12, by = 'HUC12', all.x = T, all.y = F, sort = F)
breaks <- merge( x = breaks, y = meanBPHUC8 , by = 'HUC8' , all.x = T, all.y = F, sort = F)
breaks <- merge( x = breaks, y = meanBPHUC4 , by = 'HUC4' , all.x = T, all.y = F, sort = F)

# Add columns for final breakpoints
breaks$finalSpringBP  <- NA
breaks$sourceSpringBP <- NA
breaks$finalFallBP    <- NA
breaks$sourceFallBP   <- NA

# Calculated BPs
# --------------
# Spring
newSpringBP <- which(is.na(breaks$finalSpringBP) & !is.na(breaks$springBP) )
breaks$finalSpringBP [ newSpringBP ] <- breaks$springBP[ newSpringBP ]
breaks$sourceSpringBP[ newSpringBP ] <- 'directly calculated'

#Fall
newFallBP <- which(is.na(breaks$finalFallBP) & !is.na(breaks$fallBP) )
breaks$finalFallBP [ newFallBP ] <- breaks$fallBP[ newFallBP ]
breaks$sourceFallBP[ newFallBP ] <- 'directly calculated'

# Site averaged BPs
# -----------------
# Spring
siteBP <- which(is.na(breaks$finalSpringBP) & !is.na(breaks$meanSpringBPSite) )
breaks$finalSpringBP [ siteBP ] <- breaks$meanSpringBPSite[ siteBP ]
breaks$sourceSpringBP[ siteBP ] <- 'site mean'

# Fall
siteBP <- which(is.na(breaks$finalFallBP) & !is.na(breaks$meanFallBPSite) )
breaks$finalFallBP [ siteBP ] <- breaks$meanFallBPSite[ siteBP ]
breaks$sourceFallBP[ siteBP ] <- 'site mean'


# HUC12 averaged BPs
# ------------------
# Spring
huc12BP <- which(is.na(breaks$finalSpringBP) & !is.na(breaks$meanSpringBPHUC12) )
breaks$finalSpringBP [ huc12BP ] <- breaks$meanSpringBPHUC12[ huc12BP ]
breaks$sourceSpringBP[ huc12BP ] <- 'HUC12 mean'

# Fall
huc12BP <- which(is.na(breaks$finalFallBP) & !is.na(breaks$meanFallBPHUC12) )
breaks$finalFallBP [ huc12BP ] <- breaks$meanFallBPHUC12[ huc12BP ]
breaks$sourceFallBP[ huc12BP ] <- 'HUC12 mean'

# HUC8 averaged BPs
# -----------------
# Spring
huc8BP <- which(is.na(breaks$finalSpringBP) & !is.na(breaks$meanSpringBPHUC8) )
breaks$finalSpringBP [ huc8BP ] <- breaks$meanSpringBPHUC8[ huc8BP ]
breaks$sourceSpringBP[ huc8BP ] <- 'HUC8 mean'

# Fall
huc8BP <- which(is.na(breaks$finalFallBP) & !is.na(breaks$meanFallBPHUC8) )
breaks$finalFallBP [ huc8BP ] <- breaks$meanFallBPHUC8[ huc8BP ]
breaks$sourceFallBP[ huc8BP ] <- 'HUC8 mean'

# HUC4 averaged BPs
# -----------------
# Spring
huc4BP <- which(is.na(breaks$finalSpringBP) & !is.na(breaks$meanSpringBPHUC4) )
breaks$finalSpringBP [ huc4BP ] <- breaks$meanSpringBPHUC4[ huc4BP ]
breaks$sourceSpringBP[ huc4BP ] <- 'HUC4 mean'

# Fall
huc4BP <- which(is.na(breaks$finalFallBP) & !is.na(breaks$meanFallBPHUC4) )
breaks$finalFallBP [ huc4BP ] <- breaks$meanFallBPHUC4[ huc4BP ]
breaks$sourceFallBP[ huc4BP ] <- 'HUC4 mean'

# The above data or kriging or something will have to be used to establish the syncronized portion of the year when doing the predictions to all catchments in all years after the model is run. Originally there was a model for the breakpoints but it used all the same covariates as the model of the temperature data, which seemed potentially problematic.

### Save the output

# Index the columns to save
springFallBPs <- breaks[,c('site', 'year', 'finalSpringBP', 'sourceSpringBP', 'finalFallBP', 'sourceFallBP')]

head(springFallBPs)
str(springFallBPs)

# Save the output
# save(springFallBPs, file = paste0(dataOutDir, outFile, '.RData'))
saveRDS(springFallBPs, file=output_file)

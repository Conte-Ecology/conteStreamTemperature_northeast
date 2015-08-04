# Merge temperatureData and climateData to create masterData
#
# usage: $ Rscript create_masterData.R <input temperatureData RData> <input climateData RData> <output masterData RData>
# example: $ Rscript create_masterData.R ./temperatureData.RData ./climateData.RData ./masterData.RData

library(lubridate)
library(dplyr)

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
temperatureData_file <- args[1]
if (!file.exists(temperatureData_file)) {
  stop(paste0('Could not find temperatureData file: ', temperatureData_file))
}
climateData_file <- args[2]
if (!file.exists(climateData_file)) {
  stop(paste0('Could not find climateData file: ', climateData_file))
}
masterData_file <- args[3]
if (file.exists(masterData_file)) {
  warning(paste0('Overwriting output RData file: ', masterData_file))
}

# load data frames
temperatureData <- readRDS('localData/temperatureData.RData')
climateData <- readRDS('localData/climateData.RData')

# merge
masterData <- left_join(temperatureData, climateData)

# save to RData file
saveRDS(masterData, file=masterData_file)

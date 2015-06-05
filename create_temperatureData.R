# Convert temperatureData csv file to RData file
#
# usage: $ Rscript create_temperatureData.R <input temperatureData csv> <output temperatureData RData>
# example: $ Rscript create_temperatureData.R ./temperatureData.csv ./temperatureData.RData

library(lubridate)
library(dplyr)

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
csv_file <- args[1]
if (!file.exists(csv_file)) {
  stop(paste0('Could not find temperatureData csv file: ', csv_file))
}
rdata_file <- args[2]
if (file.exists(rdata_file)) {
  warning(paste0('Overwriting output RData file: ', rdata_file))
}

# read csv
temperatureData <- read.csv(csv_file, stringsAsFactors=FALSE)

# parse dates
temperatureData <- mutate(temperatureData, date=ymd(date))

# save to RData file
saveRDS(temperatureData, file=rdata_file)

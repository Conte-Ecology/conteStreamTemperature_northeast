# Convert covariateData csv file to RData file
#
# usage: $ Rscript create_covariateData.R <input covariateData csv> <output covariateData RData>
# example: $ Rscript create_covariateData.R ./covariateData.csv ./covariateData.RData

library(lubridate)
library(dplyr)

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
csv_file <- args[1]
if (!file.exists(csv_file)) {
  stop(paste0('Could not find covariateData csv file: ', csv_file))
}
rdata_file <- args[2]
if (file.exists(rdata_file)) {
  warning(paste0('Overwriting output RData file: ', rdata_file))
}

# read csv file
covariateData <- read.csv(csv_file, stringsAsFactors=FALSE)

# convert HUCs to strings with padded zeros
covariateData <- mutate(covariateData,
                        HUC4=sprintf('%04d', HUC4),
                        HUC8=sprintf('%08d', HUC8),
                        HUC12=as.character(HUC12), # too big for integer
                        HUC12=ifelse(nchar(HUC12)==11, paste0("0", HUC12), HUC12))

# save RData file
saveRDS(covariateData, file=rdata_file)

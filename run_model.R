# Run model and save JAGS output
# requires tempDataSync input binary file
# saves output M.huc to binary file
#
# usage: $ Rscript run_model.R <input tempDataSync rdata> <output jags rdata> <output covariate list rdata>
# example: $ Rscript run_model.R ./tempDataSync.RData ./jags.RData ./covariate-list.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
tempDataSync_file <- args[1]
if (!file.exists(tempDataSync_file)) {
  stop(paste0('Could not find tempDataSync binary file: ', tempDataSync_file))
}
load(tempDataSync_file)

output_file <- args[2]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

output2_file <- args[3]
if (file.exists(output2_file)) {
  warning(paste0('Output2 file already exists, overwriting: ', output2_file))
}

# ----
### Run the model in JAGS

library(dplyr)
#library(nlme)
library(devtools)
# install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

fixed.ef <- c("intercept" 
              , "Latitude" 
              , "Longitude" 
              , "TotDASqKM" 
              , "Forest" 
              , "ReachElevationM" 
              , "SurficialCoarseC" 
              , "CONUSWetland" 
              , "ImpoundmentsAllSqKM" 
              , "airTemp.TotDASqKM"
)

site.ef <- c( "intercept.site" 
              , "airTemp" 
              , "airTempLagged2" 
              , "prcp" 
              , "prcpLagged1" 
              , "airTemp.prcp"
)

year.ef <- c( "intercept.year"
              , "dOY" 
              , "dOY2"
              , "dOY3"
)

cov.list <- list(fixed.ef = fixed.ef, site.ef = site.ef, huc.ef = site.ef, year.ef = year.ef)
# model matrix not working because creates a design matrix

#data.cal <- prepDF(tempDataSyncS, formulae = formulae)
#str(data.cal)

monitor.params <- c(#"residuals",
  #"deviance",
  "sigma",
  "B.ar1",
  "mu.ar1",
  "sigma.ar1",
  "B.0",
  "B.site",
  "rho.B.site",
  "mu.site",
  "sigma.b.site",
  "B.huc",
  "rho.B.huc",
  "mu.huc",
  "sigma.b.huc",
  "B.year",
  "rho.B.year",
  "mu.year",
  "sigma.b.year")

coda.tf <- T # currently only works in full for TRUE (using coda.samples)

# create code directory if doesn't exist
if (!file.exists('code')) {
  dir.create('code')
}

system.time(M.ar1 <- modelRegionalTempAR1(tempDataSyncS, cov.list, firstObsRows = firstObsRows, evalRows = evalRows, n.burn = 1000, n.it = 1000, n.thin = 1, nc = 3, coda = coda.tf, param.list = monitor.params)) # Slow with AR1: ~3-6 min per 100 iterations (13 min per 100 iter for site AR)

# save to rdata
saveRDS(M.ar1, file = output_file)
saveRDS(cov.list, file = output2_file)

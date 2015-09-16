# Run model and save JAGS output
# requires tempDataSync input binary file
# saves output M.huc to binary file
#
# usage: $ Rscript run_model.R <input tempDataSync rdata> <output jags rdata> <output covariate list rdata>
# example: $ Rscript run_model.R ./tempDataSync.RData ./jags.RData ./covariate_list.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

gc()

library(dplyr)
library(devtools)
install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)

data_dir <- "localData_2015-08-24"

# parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/tempDataSync.RData"), paste0(data_dir, "/jags.RData"), paste0(data_dir, "/covariate_list.RData")) # "localData/covariateData.RData",
}

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

### Run the model in JAGS

fixed.ef <- c("intercept" 
              , "prcp2"
              , "AreaSqKM"
              , "prcp2.da"
              , "airTemp.prcp2.da"
              , "forest"
              , "airTemp.forest"
              #, "airTemp.prcp2.da.forest" # maybe add in when have riparian forest
              #, "temp7p.prcp7.da"
              #, "temp7p.forest.prcp7.da"
              , "devel_hi"
              , "airTemp.devel_hi"
              , "prcp30"
              , "prcp30.da"
              , "airTemp.prcp30.da"
             , "allonnet"
             , "allonnet2"
             , "airTemp.allonnet"
             , "airTemp.allonnet2"
             , "agriculture" # consider making random when extend to VA
             , "airTemp.agriculture" # try making random when extend to VA
)

site.ef <- c( "intercept.site" 
              , "airTemp"
              , "temp7p"
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
  #"rho.B.site",
  "stream.mu",
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
if (!file.exists(paste0(data_dir, '/code'))) {
  dir.create(paste0(data_dir, '/code'))
}

system.time(M.ar1 <- modelRegionalTempAR1(tempDataSyncS, cov.list=cov.list, firstObsRows = firstObsRows, evalRows = evalRows, n.burn = 100, n.it = 100, n.thin = 1, nc = 3, coda = coda.tf, param.list = monitor.params)) # Slow with AR1: ~13 min per 100 iter for site AR = 13 hours for 6000 iterations

# save to rdata
saveRDS(M.ar1, file = output_file)
saveRDS(cov.list, file = output2_file)

rm(list = ls())
gc()

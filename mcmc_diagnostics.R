# Summarize model
# requires jags input binary file (M.ar1 dataframe), covariate-list (cov.list), and tempDataSync
# saves output coef.list to binary file coef.RData
#
# usage: $ Rscript summarize_model.R <input tempDataSync rdata> <input jags rdata> <input cov.list rdata> <output coef rdata>
# example: $ Rscript summarize_model.R ./tempDataSync.RData ./jags.RData ./covariate-list.RData ./coef.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments
rm(list = ls())
gc()

detach("package:dplyr", unload = TRUE)

library(ggmcmc)
#library(dplyr)
library(rjags)

data_dir <- "localData_2016-01-19" 

args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/tempDataSync.RData"), paste0(data_dir, "/jags.RData")) # "localData/covariateData.RData",
}

tempDataSync_file <- args[1]
if (!file.exists(tempDataSync_file)) {
  stop(paste0('Could not find tempDataSync binary file: ', tempDataSync_file))
}
load(tempDataSync_file)

jags_file <- args[2]
if (!file.exists(jags_file)) {
  stop(paste0('Could not find jags binary file: ', jags_file))
}
M.ar1 <- readRDS(jags_file)

covlist_file <- args[3]
if (!file.exists(covlist_file)) {
  stop(paste0('Could not find covariate-list binary file: ', covlist_file))
}
cov.list <- readRDS(covlist_file)

output_file <- args[4]
if (file.exists(output_file)) {
  warning(paste0('Output file already exists, overwriting: ', output_file))
}

# ----

if (!file.exists(paste0(data_dir, "/figures/"))) {
  dir.create(paste0(data_dir, "/figures/"))
}

# JAGS object is too large, need to cut daily predictions
mcmc_small <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][1:3000, which(!grepl("stream.mu", bar) & !grepl("trend", bar))]
  mcmc_small[[i]] <- as.mcmc(sna)
}

reject <- rejectionRate(mcmc_small)

mcmc_tiny <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][1:3000, which(grepl("B.0", bar) | grepl("sigma", bar))]
  mcmc_tiny[[i]] <- as.mcmc(sna)
}
gelman.diag(mcmc_tiny)

system.time(ggs.ar1 <- ggs(mcmc_small)) 

gc(verbose = FALSE) 

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4) #, simplify_traceplot = 50)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

#ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-year.pdf"), family = "mu.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

#ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-ar1.pdf"), family = "mu.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

#ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-ar1.pdf"), family = "sigma.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

detach("package:ggmcmc", unload = TRUE)



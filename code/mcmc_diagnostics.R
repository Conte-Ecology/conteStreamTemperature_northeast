# Summarize model
# requires jags input binary file (M.ar1 dataframe)
#
# usage: $ Rscript mcmc_diagnostics.R <input jags rdata>
# example: $ Rscript mcmc_diagnostics ./jags.RData 

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments
rm(list = ls())
gc()

# detach("package:dplyr", unload = TRUE)

library(ggmcmc)
#library(dplyr)
library(rjags)

# get current model run directory
data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1]) 

args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/jags.RData")) # "localData/covariateData.RData",
}

jags_file <- args[1]
if (!file.exists(jags_file)) {
  stop(paste0('Could not find jags binary file: ', jags_file))
}
M.ar1 <- readRDS(jags_file)

# ----

if (!file.exists(paste0(data_dir, "/figures/"))) {
  dir.create(paste0(data_dir, "/figures/"))
}


#--------- basic ---------
if(FALSE) { # shut off unless running manually
  # plot(M.ar1[ , c("xi.huc[1]", "xi.huc[2]", "xi.huc[3]")])
  # plot(M.ar1[ , c("xi.year[1]", "xi.year[2]")])
  # plot(M.ar1[ , c("xi.year[3]", "xi.year[4]")])
  
  plot(M.ar1[ , c("B.0[1]", "sigma.b.huc[2]", "sigma.b.huc[3]")])
  plot(M.ar1[ , c("sigma.b.site[1]", "sigma.b.site[2]", "sigma.b.site[3]")])
  plot(M.ar1[ , c("mu.huc[2]", "mu.huc[3]")])
  #plot(M.ar1[ , c("sigma.b.site[4]", "sigma.b.site[5]", "sigma.b.site[6]")])
  plot(M.ar1[ , c("B.0[2]", "B.0[3]", "B.0[4]")])
  plot(M.ar1[ , c("sigma.b.year")])
  #plot(M.ar1[ , c("sigma.b.year[2]", "sigma.b.year[3]", "sigma.b.year[4]")])
  plot(M.ar1[ , c("sigma")])
  
  plot(M.ar1[ , c("tau.B.huc.raw[1,1]", "tau.B.huc.raw[2,2]", "tau.B.huc.raw[3,3]")])
  #plot(M.ar1[ , c("tau.B.year.raw[1,1]", "tau.B.year.raw[2,2]", "tau.B.year.raw[3,3]", "tau.B.year.raw[4,4]")])
  
  # plot(M.ar1[ , c("trend.reduced[1]", "trend.reduced[2]", "trend.reduced[100]")])
  # plot(M.ar1[ , c("pred.reduced[2]", "pred.reduced[20]", "pred.reduced[100]")])
  
  plot(M.ar1[ , c("trend[1]", "trend[300]", "trend[600]")])
  plot(M.ar1[ , c("stream.mu[2]", "stream.mu[400]", "stream.mu[1000]")])
}

#-------------------------
# JAGS object is too large, need to cut daily predictions
mcmc_small <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][ , which(!grepl("stream.mu", bar) & !grepl("trend", bar))]
  mcmc_small[[i]] <- as.mcmc(sna)
}

reject <- rejectionRate(mcmc_small)

mcmc_tiny <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][ , which(grepl("B.0", bar) | grepl("sigma", bar))]
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



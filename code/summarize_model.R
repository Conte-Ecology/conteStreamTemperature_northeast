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

library(ggplot2)
library(ggmcmc)
library(dplyr)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(rjags)

data_dir <- "localData_2016-02-26_newDelineation" 

args <- commandArgs(trailingOnly = TRUE)

if(length(args) < 1) {
  args <- c(paste0(data_dir, "/tempDataSync.RData"), paste0(data_dir, "/jags.RData"), paste0(data_dir, "/covariate_list.RData"), paste0(data_dir, "/coef.RData")) # "localData/covariateData.RData",
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


test_jags_pred <- TRUE
if(test_jags_pred) {
  #plot(M.ar1[ , 1])
  mat.ar1 <- as.matrix(M.ar1) # massive
  str(mat.ar1)
  # need to use regex to rename coefficients
  temp.predicted.mean <- as.data.frame(mat.ar1) %>%
    dplyr::select(contains("stream.mu")) %>%
    colMeans()
  temps <- data.frame(tempDataSyncS$temp, temp.predicted.mean)
  names(temps) <- c("temp", "temp.predicted")
  
  ggplot(temps, aes(temp, temp.predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "blue")
  
  rmse(temps$temp - temps$temp.predicted) # 0.633
}

# JAGS object is too large, need to cut daily predictions
fu <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][1:nrow(M.ar1[[1]]), which(!grepl("stream.mu", bar) & !grepl("trend", bar))]
  fu[[i]] <- as.mcmc(sna)
}

system.time(ggs.ar1 <- ggs(fu)) 

gc(verbose = FALSE) 

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-year.pdf"), family = "mu.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-ar1.pdf"), family = "mu.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-ar1.pdf"), family = "sigma.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = "ggs_traceplot")

ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation"))

detach("package:ggmcmc", unload = TRUE)







system.time(coef.summary <- avgCoefs(ggs.ar1)) 

gc(verbose = FALSE)

B.fixed <- dplyr::filter(coef.summary, grepl('^B.0', coef.summary$Parameter))
B.fixed$coef <- cov.list$fixed.ef

mu.huc <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$HUC8)), family = "mu.huc", name = "coef", conditional = FALSE, coefs = cov.list$site.ef)

mu.year <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$year)), family = "mu.year", name = "coef", conditional = FALSE, coefs = cov.list$year.ef)

mu.ar1 <- dplyr::filter(coef.summary, grepl('^mu.ar1', coef.summary$Parameter))
mu.ar1$coef <- mu.ar1$Parameter

sigma.b.site <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$site)), family = "sigma.b.site", conditional = FALSE, coefs = cov.list$site.ef, name = "coef")

sigma.b.huc <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$site)), family = "sigma.b.huc", conditional = FALSE, coefs = cov.list$site.ef, name = "coef")

sigma.b.year <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$year)), family = "sigma.b.year", conditional = FALSE, coefs = cov.list$year.ef, name = "coef")

sigma.ar1 <- dplyr::filter(coef.summary, grepl('^sigma.ar1', coef.summary$Parameter))

B.ar1 <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$site)), family = 'B.ar1', name = "site")

B.site <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$site)), family = "B.site", coefs = cov.list$site.ef, name = "site")

B.huc <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$HUC8)), family = "B.huc", coefs = cov.list$site.ef, name = "huc")

B.year <- nameCoefs(coef.summary = coef.summary, rand.levels = levels(as.factor(tempDataSyncS$year)), family = "B.year", coefs = cov.list$year.ef, name = "year")


# Make correlation matrix of random huc effects
cor.huc <- as.data.frame(matrix(NA, length(cov.list$site.ef), length(cov.list$site.ef)))
names(cor.huc) <- cov.list$site.ef
row.names(cor.huc) <- cov.list$site.ef
for(k in 1:length(cov.list$site.ef)){
  for(k.prime in 1:length(cov.list$site.ef)){
    cor.huc[k, k.prime] <- coef.summary[which(coef.summary$Parameter == paste('rho.B.huc[',k,',',k.prime,']', sep="")), "mean"]
  }
}
cor.huc <- round(cor.huc, digits=3)
cor.huc[upper.tri(cor.huc, diag=TRUE)] <- ''
cor.huc

# Make correlation matrix of random year effects
cor.year <- as.data.frame(matrix(NA, length(cov.list$year.ef), length(cov.list$year.ef)))
names(cor.year) <- cov.list$year.ef
row.names(cor.year) <- cov.list$year.ef
for(l in 1:length(cov.list$year.ef)){
  for(l.prime in 1:length(cov.list$year.ef)){
    cor.year[l, l.prime] <- coef.summary[which(coef.summary$Parameter == paste('rho.B.year[',l,',',l.prime,']', sep="")), "mean"]
  }
}
cor.year <- round(cor.year, digits=3)
cor.year[upper.tri(cor.year, diag=TRUE)] <- ''
cor.year


fix.ef <- rbind(B.fixed, select(mu.huc, -index), select(mu.year, -index), mu.ar1)

coef.list <- list(fix.ef = fix.ef
                  ,B.fixed = B.fixed
                  , mu.huc = mu.huc
                  , mu.year = mu.year
                  , mu.ar1 = mu.ar1
                  , sigma.site = sigma.b.site
                  , sigma.huc = sigma.b.huc
                  , sigma.year = sigma.b.year
                  , sigma.ar1 = sigma.ar1
                  , cor.huc = cor.huc
                  , cor.year = cor.year
                  , B.site = B.site
                  , B.huc = B.huc
                  , B.year = B.year
                  , B.ar1 = B.ar1
)

# save(modSummary, file=paste0(dataOutDir, 'modSummary.RData'))
#output_file <- "localData/coef.RData"
#saveRDS(coef.list, file=output_file)

# Summarize model
# requires jags input binary file (M.ar1 dataframe), covariate-list (cov.list), and tempDataSync
# saves output coef.list to binary file coef.RData
#
# usage: $ Rscript summarize_model.R <input tempDataSync rdata> <input jags rdata> <input cov.list rdata> <output coef rdata>
# example: $ Rscript summarize_model.R ./tempDataSync.RData ./jags.RData ./covariate_list.RData ./coef.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments

library(ggplot2)
library(dplyr)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(rjags)

# get current model run directory
data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1])

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

########## Convert JAGS Output #########

df.ar1 <- as.data.frame(as.matrix(M.ar1)) # massive

test_jags_pred <- FALSE
if(test_jags_pred) {
  #plot(M.ar1[ , 1])
  #str(df.ar1)
  # need to use regex to rename coefficients
  temp.predicted.mean <- df.ar1 %>%
    dplyr::select(starts_with("stream.mu")) %>%
    colMeans()
  temps <- data.frame(tempDataSyncS$temp, temp.predicted.mean)
  names(temps) <- c("temp", "temp.predicted")
  
  g <- ggplot(temps, aes(temp, temp.predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "blue")
  print(g)
  rmse(temps$temp - temps$temp.predicted) # 0.645
}

#mat2 <- dplyr::select(df.ar1, -starts_with("stream.mu"))
#dim(mat2)
#str(mat2)

cov.list

#str(coef.list)

########## raw tau and xi ##########

# tau.b.year.raw <- df.ar1 %>%
#   dplyr::select(starts_with("tau.B.year.raw")) %>%
#   colMeans()
# tau.b.year.raw <- as.data.frame(matrix(tau.b.year.raw, sqrt(length(tau.b.year.raw)), sqrt(length(tau.b.year.raw))))
# names(tau.b.year.raw) <- cov.list$year.ef
# row.names(tau.b.year.raw) <- cov.list$year.ef
# tau.b.year.raw <- round(tau.b.year.raw, digits=3)
# tau.b.year.raw

tau.b.huc.raw <- df.ar1 %>%
  dplyr::select(starts_with("tau.B.huc.raw")) %>%
  colMeans()
tau.b.huc.raw <- as.data.frame(matrix(tau.b.huc.raw, sqrt(length(tau.b.huc.raw)), sqrt(length(tau.b.huc.raw))))
names(tau.b.huc.raw) <- cov.list$huc.ef
row.names(tau.b.huc.raw) <- cov.list$huc.ef
tau.b.huc.raw <- round(tau.b.huc.raw, digits=3)
tau.b.huc.raw

# xi.huc <- df.ar1 %>%
#   dplyr::select(starts_with("xi.huc")) %>%
#   colMeans()
# xi.huc

# xi.year <- df.ar1 %>%
#   dplyr::select(starts_with("xi.year")) %>%
#   colMeans()
# xi.year

# saveRDS(list(tau.b.huc.raw=tau.b.huc.raw, tau.b.year.raw=tau.b.year.raw, xi.huc=xi.huc, xi.year=xi.year), file = paste0(data_dir, "/raw_scale_est.RData"))

########## Fixed Effects ##############
# Extract fixed effects coefficients (all iterations as rows)
B.0 <- df.ar1 %>%
  dplyr::select(starts_with("B.0"))# %>%
#dplyr::mutate(iter = 1:nrow(df_ar1))
B.0.fixed <- data.frame(coef = cov.list$fixed.ef)
B.0.fixed$mean <- colMeans(B.0)
B.0.fixed$sd <- apply(B.0, MARGIN = 2, sd, na.rm=T)
B.0.fixed$LCRI <- apply(B.0, MARGIN = 2, function(x) quantile(x, probs = 0.025, na.rm=T))
B.0.fixed$UCRI <- apply(B.0, MARGIN = 2, function(x) quantile(x, probs = 0.975, na.rm=T))
B.0.fixed

######## Random Site Effects #########
# extract site effect coefficients
B.site1 <- df.ar1 %>%
  dplyr::select(starts_with("B.site"))
str(B.site1)

# organize: row = site, column = site covariate, list position = iteration
B.site.list <- lapply(seq_len(nrow(B.site1)), function(j) B.site1[j, ])
sites <- unique(as.character(tempDataSyncS$site))
B.site <- as.data.frame(matrix(NA, nrow = length(rand_ids$df_site$sitef), ncol = length(cov.list$site.ef) + 1))
names(B.site) <- c("sitef", paste("B.site", cov.list$site.ef, sep = "_"))
B.site$sitef <- rand_ids$df_site$sitef
for(j in 1:length(B.site.list)) {
  for(i in 1:length(cov.list$site.ef)) { #loop through coefficients
    B.site[ , i+1] <- as.numeric(unname(t(dplyr::select(B.site.list[[j]], ends_with(paste0(i, "]"))))))
  }
  B.site.list[[j]] <- B.site
}

# convert to array then get summary of each site
n.iter <- length(B.site.list)
n.site.params <- ncol(B.site.list[[1]])
n.sites <- length(unique(sites))

foo <- array(unlist(B.site.list), dim = c(n.sites, n.site.params, n.iter))
B.site.means.site <- apply(foo, 1:2, mean, na.rm = TRUE) # not sure if I will use this but will be what's used for web app predictions
#B.site.means.test <- colMeans(B.site.means.site[ , -1]) # same as B.site.means

# get means for each coef within each iteration
B.site.list.means <- lapply(B.site.list, colMeans, na.rm = T)
foo <- matrix(unlist(B.site.list.means), ncol = length(B.site.list.means[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.site.list[[1]])[-1]
B.site.means <- data.frame(coef = names(B.site.list[[1]])[-1])
B.site.means$mean <- colMeans(foo, na.rm = T) # should be ~0
B.site.means$sd <- apply(foo, 2, sd, na.rm = T)
B.site.means$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
B.site.means$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))

# standard deviations
B.site.list.sd <- lapply(B.site.list, FUN = function(x) apply(x, 2, sd, na.rm = T))
foo <- matrix(unlist(B.site.list.sd), ncol = length(B.site.list.sd[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.site.list[[1]])[-1]
sigma.b.site <- data.frame(coef = names(B.site.list[[1]])[-1])
sigma.b.site$mean.sd <- colMeans(foo, na.rm = T)
sigma.b.site$sd <- apply(foo, 2, sd, na.rm = T)
sigma.b.site$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
sigma.b.site$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))


######## Random HUC Effects #########
# extract huc effect coefficients
B.huc1 <- df.ar1 %>%
  dplyr::select(starts_with("B.huc"))
str(B.huc1)

# organize: row = huc, column = huc covariate, list position = iteration
B.huc.list <- lapply(seq_len(nrow(B.huc1)), function(j) B.huc1[j, ])
hucs <- unique(as.character(tempDataSyncS$huc))
B.huc <- as.data.frame(matrix(NA, nrow = length(rand_ids$df_huc$hucf), ncol = length(cov.list$huc.ef) + 1))
names(B.huc) <- c("hucf", paste("B.huc", cov.list$huc.ef, sep = "_"))
B.huc$hucf <- rand_ids$df_huc$hucf
for(j in 1:length(B.huc.list)) {
  for(i in 1:length(cov.list$huc.ef)) { #loop through coefficients
    B.huc[ , i+1] <- as.numeric(unname(t(dplyr::select(B.huc.list[[j]], ends_with(paste0(i, "]"))))))
  }
  B.huc.list[[j]] <- B.huc
}

# convert to array then get summary of each huc
n.iter <- length(B.huc.list)
n.huc.params <- ncol(B.huc.list[[1]])
n.hucs <- length(unique(hucs))

foo <- array(unlist(B.huc.list), dim = c(n.hucs, n.huc.params, n.iter))
B.huc.means.huc <- apply(foo, 1:2, mean, na.rm = TRUE) # not sure if I will use this but will be what's used for web app predictions
#B.huc.means.test <- colMeans(B.huc.means.huc[ , -1]) # same as B.huc.means

# get means for each coef within each iteration
B.huc.list.means <- lapply(B.huc.list, colMeans, na.rm = T)
foo <- matrix(unlist(B.huc.list.means), ncol = length(B.huc.list.means[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.huc.list[[1]])[-1]
mu.huc <- data.frame(coef = names(B.huc.list[[1]])[-1])
mu.huc$mean <- colMeans(foo, na.rm = T) # should be ~0
mu.huc$sd <- apply(foo, 2, sd, na.rm = T)
mu.huc$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
mu.huc$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))

# standard deviations
B.huc.list.sd <- lapply(B.huc.list, FUN = function(x) apply(x, 2, sd, na.rm = T))
foo <- matrix(unlist(B.huc.list.sd), ncol = length(B.huc.list.sd[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.huc.list[[1]])[-1]
sigma.b.huc <- data.frame(coef = names(B.huc.list[[1]])[-1])
sigma.b.huc$mean.sd <- colMeans(foo, na.rm = T)
sigma.b.huc$sd <- apply(foo, 2, sd, na.rm = T)
sigma.b.huc$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
sigma.b.huc$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))

######## Random year Effects #########
if(length(cov.list$year.ef) == 1) {
  # extract year effect coefficients
  B.year1 <- df.ar1 %>%
    dplyr::select(starts_with("B.year"))
  str(B.year1)
  
  # organize: row = year, column = year covariate, list position = iteration
  B.year.list <- lapply(seq_len(nrow(B.year1)), function(j) B.year1[j, ])
  years <- unique(as.character(tempDataSyncS$year))
  B.year <- as.data.frame(matrix(NA, nrow = length(rand_ids$df_year$yearf), ncol = length(cov.list$year.ef) + 1))
  names(B.year) <- c("yearf", paste("B.year", cov.list$year.ef, sep = "_"))
  B.year$yearf <- rand_ids$df_year$yearf
  for(j in 1:length(B.year.list)) {
    for(i in 1:length(cov.list$year.ef)) { #loop through coefficients
      B.year[ , i+1] <- as.numeric(unname(t(dplyr::select(B.year.list[[j]], ends_with(paste0(i, "]"))))))
    }
    B.year.list[[j]] <- B.year
  }
  
  # convert to array then get summary of each year
  n.iter <- length(B.year.list)
  n.year.params <- ncol(B.year.list[[1]])
  n.years <- length(unique(years))
  
  foo <- array(unlist(B.year.list), dim = c(n.years, n.year.params, n.iter))
  B.year.means.year <- apply(foo, 1:2, mean, na.rm = TRUE) # not sure if I will use this but will be what's used for web app predictions
  #B.year.means.test <- colMeans(B.year.means.year[ , -1]) # same as B.year.means
  
  # get means for each coef within each iteration
  B.year.list.means <- lapply(B.year.list, colMeans, na.rm = T)
  foo <- as.data.frame(matrix(unlist(B.year.list.means), ncol = length(B.year.list.means[[1]]), byrow = TRUE)[ , -1])
  colnames(foo) <- names(B.year.list[[1]])[-1]
  B.year.means <- data.frame(coef = names(B.year.list[[1]])[-1])
  B.year.means$mean <- colMeans(foo, na.rm = T) # should be ~0
  B.year.means$sd <- apply(foo, 2, sd, na.rm = T)
  B.year.means$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
  B.year.means$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))
  
  # standard deviations
  B.year.list.sd <- lapply(B.year.list, FUN = function(x) apply(x, 2, sd, na.rm = T))
  foo <- as.data.frame(matrix(unlist(B.year.list.sd), ncol = length(B.year.list.sd[[1]]), byrow = TRUE)[ , -1])
  colnames(foo) <- names(B.year.list[[1]])[-1]
  sigma.b.year <- data.frame(coef = names(B.year.list[[1]])[-1])
  sigma.b.year$mean.sd <- colMeans(foo, na.rm = T)
  sigma.b.year$sd <- apply(foo, 2, sd, na.rm = T)
  sigma.b.year$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
  sigma.b.year$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))
  
} else {
# extract year effect coefficients
B.year1 <- df.ar1 %>%
  dplyr::select(starts_with("B.year"))
str(B.year1)

# organize: row = year, column = year covariate, list position = iteration
B.year.list <- lapply(seq_len(nrow(B.year1)), function(j) B.year1[j, ])
years <- unique(as.character(tempDataSyncS$year))
B.year <- as.data.frame(matrix(NA, nrow = length(rand_ids$df_year$yearf), ncol = length(cov.list$year.ef) + 1))
names(B.year) <- c("yearf", paste("B.year", cov.list$year.ef, sep = "_"))
B.year$yearf <- rand_ids$df_year$yearf
for(j in 1:length(B.year.list)) {
  for(i in 1:length(cov.list$year.ef)) { #loop through coefficients
    B.year[ , i+1] <- as.numeric(unname(t(dplyr::select(B.year.list[[j]], ends_with(paste0(i, "]"))))))
  }
  B.year.list[[j]] <- B.year
}

# convert to array then get summary of each year
n.iter <- length(B.year.list)
n.year.params <- ncol(B.year.list[[1]])
n.years <- length(unique(years))

foo <- array(unlist(B.year.list), dim = c(n.years, n.year.params, n.iter))
B.year.means.year <- apply(foo, 1:2, mean, na.rm = TRUE) # not sure if I will use this but will be what's used for web app predictions
#B.year.means.test <- colMeans(B.year.means.year[ , -1]) # same as B.year.means

# get means for each coef within each iteration
B.year.list.means <- lapply(B.year.list, colMeans, na.rm = T)
foo <- matrix(unlist(B.year.list.means), ncol = length(B.year.list.means[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.year.list[[1]])[-1]
mu.year <- data.frame(coef = names(B.year.list[[1]])[-1])
mu.year$mean <- colMeans(foo, na.rm = T) # should be ~0
mu.year$sd <- apply(foo, 2, sd, na.rm = T)
mu.year$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
mu.year$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))

# standard deviations
B.year.list.sd <- lapply(B.year.list, FUN = function(x) apply(x, 2, sd, na.rm = T))
foo <- matrix(unlist(B.year.list.sd), ncol = length(B.year.list.sd[[1]]), byrow = TRUE)[ , -1]
colnames(foo) <- names(B.year.list[[1]])[-1]
sigma.b.year <- data.frame(coef = names(B.year.list[[1]])[-1])
sigma.b.year$mean.sd <- colMeans(foo, na.rm = T)
sigma.b.year$sd <- apply(foo, 2, sd, na.rm = T)
sigma.b.year$LCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.025, na.rm = T))
sigma.b.year$UCRI <- apply(foo, 2, function(x) quantile(x, probs = 0.975, na.rm = T))
}

#---------- AR1 ------------
B.ar1.iter <- df.ar1 %>%
  dplyr::select(starts_with("B.ar1"))
str(B.ar1.iter)
B.ar1.iter <- as.data.frame(t(B.ar1.iter)) # site by row, iteration by column
str(B.ar1.iter)

if(FALSE) { # turn off but could add option later
######## Random AR1 Effects #########
# extract site effect coefficients
B.ar1.iter$sitef <- B.site.list[[1]]$sitef

B.ar1 <- data.frame(sitef = B.ar1.iter$sitef)
B.ar1 <- B.ar1 %>%
  dplyr::left_join(rand_ids$df_site)
B.ar1$mean <- apply(as.matrix(dplyr::select(B.ar1.iter, -sitef)), MARGIN = 1, mean, na.rm = TRUE)
B.ar1$sd <- apply(as.matrix(dplyr::select(B.ar1.iter, -sitef)), MARGIN = 1, mean, na.rm = TRUE)
B.ar1$LCRI <- apply(as.matrix(dplyr::select(B.ar1.iter, -sitef)), MARGIN = 1, quantile, probs = c(0.025), na.rm = TRUE)
B.ar1$UCRI <- apply(as.matrix(dplyr::select(B.ar1.iter, -sitef)), MARGIN = 1, quantile, probs = c(0.975), na.rm = TRUE)

mu.ar1.iter <- df.ar1 %>%
  dplyr::select(starts_with("mu.ar1"))
mu.ar1 <- data.frame(coef = "ar1", mean = mean(mu.ar1.iter$mu.ar1), sd = sd(mu.ar1.iter$mu.ar1), LCRI = quantile(mu.ar1.iter$mu.ar1, probs = c(0.025)), UCRI = quantile(mu.ar1.iter$mu.ar1, probs = c(0.975))) 

sigma.ar1 <- df.ar1 %>%
  dplyr::select(starts_with("sigma.ar1")) %>%
  colMeans()

B.ar1 <- data.frame(coef = "ar1", mean = rowMeans(B.ar1.iter[1,]), sd = sd(B.ar1.iter[1, ]), LCRI = quantile(as.numeric(B.ar1.iter[1,]), probs = c(0.025)), UCRI = quantile(as.numeric(B.ar1.iter[1,]), probs = c(0.975))) 

mu.ar1 <- B.ar1

} else {
############### if AR1 not random by site ###########
B.ar1 <- data.frame(coef = "ar1", mean = rowMeans(B.ar1.iter[1,]), sd = sd(B.ar1.iter[1, ]), LCRI = quantile(as.numeric(B.ar1.iter[1,]), probs = c(0.025)), UCRI = quantile(as.numeric(B.ar1.iter[1,]), probs = c(0.975))) 

mu.ar1 <- B.ar1
}
#####################

# cor.huc
# Make correlation matrix of random huc effects
cor.huc <- df.ar1 %>%
  dplyr::select(starts_with("rho.B.huc")) %>%
  colMeans()
cor.huc <- as.data.frame(matrix(cor.huc, length(cov.list$huc.ef), length(cov.list$huc.ef)))
names(cor.huc) <- cov.list$huc.ef
row.names(cor.huc) <- cov.list$huc.ef
cor.huc <- round(cor.huc, digits=3)
cor.huc[upper.tri(cor.huc, diag=TRUE)] <- ''
cor.huc

# Make correlation matrix of random year effects
# cor.year <- df.ar1 %>%
#   dplyr::select(starts_with("rho.B.year")) %>%
#   colMeans()
# cor.year <- as.data.frame(matrix(cor.year, length(cov.list$year.ef), length(cov.list$year.ef)))
# names(cor.year) <- cov.list$year.ef
# row.names(cor.year) <- cov.list$year.ef
# cor.year <- round(cor.year, digits=3)
# cor.year[upper.tri(cor.year, diag=TRUE)] <- ''
# cor.year


########### Make Coef List #########
mu.year <- B.year.means
fix.ef <- rbind(B.0.fixed, mu.huc, mu.year, mu.ar1)

coef.list <- list(fix.ef = fix.ef
                  ,B.fixed = B.0.fixed
                  , mu.huc = mu.huc
                  , mu.year = mu.year
                  , mu.ar1 = mu.ar1
                  , sigma.site = sigma.b.site
                  , sigma.huc = sigma.b.huc
                  , sigma.year = sigma.b.year
                  #, sigma.ar1 = sigma.ar1
                  , cor.huc = cor.huc
                  #, cor.year = cor.year
                  , B.site = B.site
                  , B.huc = B.huc
                  , B.year = B.year
                  , B.ar1 = B.ar1
)

saveRDS(coef.list, file = paste0(data_dir, "/coef.RData"))

coef.list.iters <- list(B.0=B.0, B.site.list=B.site.list, B.huc.list=B.huc.list, B.year.list=B.year.list, B.ar1.iter=B.ar1.iter, mu.huc=mu.huc, mu.year=mu.year) # mu.ar1
saveRDS(coef.list.iters, paste0(data_dir, "/coef_iters.RData"))


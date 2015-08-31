# Summarize model
# requires jags input binary file (M.ar1 dataframe), covariate-list (cov.list), and tempDataSync
# saves output coef.list to binary file coef.RData
#
# usage: $ Rscript summarize_model.R <input tempDataSync rdata> <input jags rdata> <input cov.list rdata> <output coef rdata>
# example: $ Rscript summarize_model.R ./tempDataSync.RData ./jags.RData ./covariate-list.RData ./coef.RData

# NOTE: this has not actually been run, and is mostly just copy and pasted from the analysis vignette

# parse command line arguments

library(ggplot2)
library(ggmcmc)
library(dplyr)
library(devtools)
#install_github("Conte-Ecology/conteStreamTemperature")
library(conteStreamTemperature)
library(rjags)

data_dir <- "localData_2015-08-24" 

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

test_jags_pred <- TRUE
if(test_jags_pred) {
  #plot(M.ar1[ , 1])
  #str(df.ar1)
  # need to use regex to rename coefficients
  temp.predicted.mean <- df.ar1 %>%
    dplyr::select(starts_with("stream.mu")) %>%
    colMeans()
  temps <- data.frame(tempDataSyncS$temp, temp.predicted.mean)
  names(temps) <- c("temp", "temp.predicted")
  
  ggplot(temps, aes(temp, temp.predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "blue")
  
  rmse(temps$temp - temps$temp.predicted) # 0.645
}


mat2 <- dplyr::select(df.ar1, -starts_with("stream.mu"))
dim(mat2)
str(mat2)


cov.list

#str(coef.list)

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
#sites <- unique(as.character(tempDataSyncS$site))
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
#hucs <- unique(as.character(tempDataSyncS$huc))
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
# extract year effect coefficients
B.year1 <- df.ar1 %>%
  dplyr::select(starts_with("B.year"))
str(B.year1)

# organize: row = year, column = year covariate, list position = iteration
B.year.list <- lapply(seq_len(nrow(B.year1)), function(j) B.year1[j, ])
#years <- unique(as.character(tempDataSyncS$year))
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


######## Random AR1 Effects #########
# extract site effect coefficients
B.ar1.iter <- df.ar1 %>%
  dplyr::select(starts_with("B.ar1"))
str(B.ar1.iter)
B.ar1.iter <- as.data.frame(t(B.ar1.iter)) # site by row, iteration by column
str(B.ar1.iter)
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
cor.year <- df.ar1 %>%
  dplyr::select(starts_with("rho.B.year")) %>%
  colMeans()
cor.year <- as.data.frame(matrix(cor.year, length(cov.list$year.ef), length(cov.list$year.ef)))
names(cor.year) <- cov.list$year.ef
row.names(cor.year) <- cov.list$year.ef
cor.year <- round(cor.year, digits=3)
cor.year[upper.tri(cor.year, diag=TRUE)] <- ''
cor.year



########### Make Coef List #########

fix.ef <- rbind(B.0.fixed, mu.huc, mu.year, mu.ar1)

coef.list <- list(fix.ef = fix.ef
                  ,B.fixed = B.0.fixed
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












########### Predictions ##########

data <- tempDataSyncS
df <- tempDataSyncS


# by iteration
data <- tempDataSyncValidS

#pb <- txtProgressBar(style = 3)
metrics.list <- list()
start <- Sys.time()
for(i in 1:n.iter) {
  fixed.ef <- as.numeric(B.0[i, ]) # separate out the iteration or do for mean/median
  # add specific random effects to the dataframe
 df <- data %>%
    dplyr::select(-sitef) %>%
    left_join(rand_ids$df_site) %>%
    left_join(rand_ids$df_huc) %>%
    left_join(rand_ids$df_year)
  
  
  
  df <- left_join(df, B.site.list[[i]], by = "sitef")
  df <- left_join(df, B.huc.list[[i]], by = "hucf") # problem with validation data, need to use the mean when huc don't match
  df <- left_join(df, B.year.list[[i]], by = "yearf")
  
  
  for (j in 2:length(names(B.site.list[[i]]))) {
    df[, names(B.site.list[[i]][j])][is.na(df[, names(B.site.list[[i]][j])])] <- colMeans(B.site.list[[i]][j])
  }
  for (j in 2:length(names(B.huc.list[[i]]))) {
    df[, names(B.huc.list[[i]][j])][is.na(df[, names(B.huc.list[[i]][j])])] <- colMeans(B.huc.list[[i]][j])
  }
  for (j in 2:length(names(B.year.list[[i]]))) {
    df[, names(B.year.list[[i]][j])][is.na(df[, names(B.year.list[[i]][j])])] <- colMeans(B.year.list[[i]][j])
  }
  #     dplyr::filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>%
  #dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp) %>%
    # df <- dplyr::filter(df, AreaSqKM < 200) only works on the original scale
  
  
  
  df$fixed.ef <- as.vector(fixed.ef %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef)))))))
  df$site.ef <- rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, starts_with("B.site"))))
  df$huc.ef <- rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * as.matrix(select(df, starts_with("B.huc"))))
  df$year.ef <- rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * as.matrix(select(df, starts_with("B.year"))))
  
  df$trend <- rowSums(as.matrix(dplyr::select(df, one_of(c("fixed.ef", "site.ef", "huc.ef", "year.ef")))))
  
  # Add B.ar1 to predictions
  #df <- group_by(df, sitef)
  df <- mutate(df, prev.temp = c(NA, df$temp[(2:(nrow(df))) -1]),
               prev.trend = c(NA, df$trend[(2:nrow(df)) - 1]),
               prev.err = prev.temp - prev.trend,
               tempPredicted = trend,
               prev.temp = ifelse(newDeploy == 1, NA, prev.temp),
               prev.err = ifelse(newDeploy == 1, NA, prev.err))
  B.ar1.sub <- data.frame(sitef = rand_ids$df_site$sitef)
  B.ar1.sub$B.ar1 <- as.numeric(B.ar1[ ,i]) # iteration i
  df <- left_join(df, B.ar1.sub, by = c("sitef"))
  df <- df %>%
    dplyr::mutate(B.ar1 = ifelse(is.na(B.ar1), mean(B.ar1.sub$B.ar1, na.rm = T), B.ar1)) %>%
    dplyr::arrange(featureid, date)
  
  df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), ]$prev.err
  
  #df <- dplyr::select(df, featureid, date, year, temp, trend, tempPredicted)
  
  metrics.list[[i]] <- deriveMetrics(df)
  
  cat(paste0("Iteration ", i, " of ", n.iter, ": ", Sys.time() - start, "\n"), file = paste0(data_dir, "/derived_metrics_uncertainty_loop.txt"), append = T)
  
  #names(df) <- c("featureid", "date", paste0("trend_", i), paste0("tempPredicted_", i))
  #df_preds <- dplyr::left_join(df_preds, df)
  #setTxtProgressBar(pb, i)
}
metrics.array <- array(unlist(metrics.list), dim = c(nrow(metrics.list[[1]]), ncol(metrics.list[[1]]), n.iter))
metrics.mean <- apply(metrics.array, 1:2, mean, na.rm = T)
metrics.sd <- apply(metrics.array, 1:2, sd, na.rm = T)
metrics.LCRI <- apply(metrics.array, 1:2, FUN = function(x) quantile(x, probs = c(0.025), na.rm = T))
metrics.UCRI <- apply(metrics.array, 1:2, FUN = function(x) quantile(x, probs = c(0.975), na.rm = T))
colnames(metrics.mean) <- names(metrics.list[[1]])
colnames(metrics.sd) <- names(metrics.list[[1]])
colnames(metrics.LCRI) <- names(metrics.list[[1]])
colnames(metrics.UCRI) <- names(metrics.list[[1]])
end <- Sys.time()
end - start


df_preds_summary <- df_preds %>%
  dplyr::select()

plot(df$temp, df$trend)
rmse(df$temp - df$trend)
rmse(temps$temp - temps$temp.predicted)

rmse(df$temp - df$tempPredicted)

######## Validation ##########

df <- df %>%
  dplyr::mutate(site_day = paste0(df$featureid, "_", df$date))
site_days <- unique(tempDataSyncS$site_day)
sites <- unique(tempDataSyncS$featureid)
hucs <- unique(tempDataSyncS$huc)
years <- unique(tempDataSyncS$year)

# overall validation

rmse(df$temp - df$trend)
rmse(df$temp - df$tempPredicted)

# missing days but sites, hucs, years with data
valid_miss_days <- df %>%
  dplyr::filter(!(site_day %in% site_days))

rmse(valid_miss_days$temp - valid_miss_days$trend)
rmse(valid_miss_days$temp - valid_miss_days$tempPredicted)

# missing sites but hucs and years with data

valid_miss_sites <- df %>%
  dplyr::filter(!(featureid %in% sites) & huc %in% hucs & year %in% years)

rmse(valid_miss_sites$temp - valid_miss_sites$trend)
rmse(valid_miss_sites$temp - valid_miss_sites$tempPredicted)

# sites and huc data but missing years

valid_miss_years <- df %>%
  dplyr::filter(!(year %in% years) & site %in% sites & huc %in% hucs)

rmse(valid_miss_years$temp - valid_miss_years$trend)
rmse(valid_miss_years$temp - valid_miss_years$tempPredicted)

# no data

valid_miss_all <- df %>%
  dplyr::filter(!(year %in% years) & !(site %in% sites) & !(huc %in% hucs))

rmse(valid_miss_all$temp - valid_miss_all$trend)
rmse(valid_miss_all$temp - valid_miss_all$tempPredicted)


##################
g <- ggplot(df, aes(temp, ((df$airTemp*df_stds[var.names == "airTemp", "stdevs"])+df_stds[var.names == "airTemp", "means"]))) + 
  geom_point() + 
  geom_point(aes(temp, trend), colour = "dark red", alpha = 0.5) + 
  geom_point(aes(temp, tempPredicted), colour = 'blue', alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, colour = 'black') + 
  theme_bw() + xlab("observered temperature (C)") + 
  ylab("predicted temperature (C)")

g



########## OLD Method #########
  Pred <- NA
  trend <- NA
 
    for(i in 1:length(firstObsRows)) {
        trend[firstObsRows$rowNum[i]] <- as.numeric(
          fixed.ef %*% t(as.matrix(as.data.frame(unclass(select(ungroup(df[firstObsRows[i], ]), one_of(cov.list$fixed.ef)))))) + 
                as.numeric(dplyr::filter(B.site.list[[1]], sitef == df$sitef[firstObsRows[i]])[-1]) %*% t(as.matrix(select(df[firstObsRows[i], ], one_of(cov.list$site.ef)))) +
          as.numeric(dplyr::filter(B.huc.list[[1]], hucf == df$hucf[firstObsRows[i]])[-1]) %*% t(as.matrix(select(df[firstObsRows[i], ], one_of(cov.list$huc.ef)))) +
          as.numeric(dplyr::filter(B.year.list[[1]], yearf == df$yearf[firstObsRows[i]])[-1]) %*% t(as.matrix(select(df[firstObsRows[i], ], one_of(cov.list$year.ef))))
        )
        
          Pred[firstObsRows$rowNum[i]] <- trend[firstObsRows$rowNum[i]]
    }
  
  for(i in 1:length(evalRows$rowNum)) {
        trend[evalRows$rowNum[i]] <- as.numeric(
            B.fixed$mean %*% t(df$data.fixed[evalRows$rowNum[i], ]) + 
                dplyr::filter(B.huc, huc == as.character(data$HUC8[evalRows$rowNum[i]]))$mean %*% t(df$data.random.sites[evalRows$rowNum[i], ]) + 
                dplyr::filter(B.site, site == as.character(data$site[evalRows$rowNum[i]]))$mean %*% t(df$data.random.sites[evalRows$rowNum[i], ]) + 
                dplyr::filter(B.year, year == as.character(data$year[evalRows$rowNum[i]]))$mean %*% t(df$data.random.years[evalRows$rowNum[i], ]) 
          )
        
          if(is.na(data$temp[evalRows$rowNum[i]-1]) | is.null(data$temp[evalRows$rowNum[i]-1])) {
              Pred[evalRows$rowNum[i]] <- trend[evalRows$rowNum[i]]
            } else {
                Pred[evalRows$rowNum[i]] <- trend[evalRows$rowNum[i]] + 
                    dplyr::filter(B.ar1, site == as.character(data$site[evalRows$rowNum[i]]))$mean * (data$temp[evalRows$rowNum[i]-1] - trend[evalRows$rowNum[i]-1])
              }
########################

# by mean values
trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef)))))))



# Extract fixed effect data for clarity (more memory efficient to not separate as another object)
X.0 <- as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef))))

data$trend <- as.vector(as.numeric(B.0[1, ]) %*% t(as.matrix(X.0)))


df$trend <- NA
df$trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef))))))) +
  rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, one_of(names(B.site[-1]))))) + 
  rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * as.matrix(select(df, one_of(names(B.huc[-1]))))) +
  rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * as.matrix(select(df, one_of(names(B.year[-1])))))

# Add B.ar1 to predictions
df <- mutate(df, prev.temp = c(NA, temp[(2:(nrow(data))) -1]))
df <- mutate(df, prev.trend = c(NA, trend[(2:nrow(data)) - 1]))
df <- mutate(df, prev.err = prev.temp - prev.trend)
df <- mutate(df, tempPredicted = trend)
df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), ]$prev.err






df <- prepPredictDF(data = data, coef.list = coef.list, cov.list = cov.list, var.name = "site", featureid_site = featureid_site)
df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, var.name = "huc", featureid_site = featureid_site)
df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, var.name = "year", featureid_site = featureid_site)
df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, var.name = "ar1", featureid_site = featureid_site)

str(df)

data$trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(df), one_of(cov.list$fixed.ef)))))))

site.efs <- rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, one_of(names(B.site[-1])))))





prepConditionalCoef <- function (coef.list, cov.list, var.name) 
{
  if (var.name == "ar1") {
    B <- coef.list[[paste0("B.", var.name)]]
    B$site <- as.character(B$site)
  }
  else {
    f <- paste0(var.name, " ~ coef")
    B <- dcast(coef.list[[paste0("B.", var.name)]], formula = as.formula(f), 
               value.var = "mean")
    B <- dplyr::select(B, one_of(c(var.name, cov.list[[paste0(var.name, 
                                                              ".ef")]])))
    if (var.name == "site" | var.name == "ar1") 
      B$site <- as.character(B$site)
    names(B) <- c(names(B[1]), paste0(names(B[-1]), ".B.", 
                                      var.name))
  }
  return(B)
}

data <- tempDataSyncS
var.name = "site"

prepPredictDF <- function (data, coef.list, cov.list, var.name, featureid_site) 
{
  B <- prepConditionalCoef(coef.list = coef.list, cov.list = cov.list, 
                           var.name = var.name)
  if (var.name == "site" | var.name == "ar1") {
    B$site <- as.factor(B$site)
    featureid_site$site <- as.factor(featureid_site$site)
    B <- dplyr::left_join(B, featureid_site, by = c("site"))
    if (var.name == "ar1") {
      var.name <- "site"
      data[, var.name] <- as.character(data[, var.name])
      B <- dplyr::select(B, site = site, featureid = featureid, 
                         B.ar1 = mean)
      df <- merge(data, B, by = c("featureid"), all.x = T)
      df[, names(B[-1])][is.na(df[, names(B[-1])])] <- colMeans(dplyr::select(B, 
                                                                              B.ar1))
    }
    else {
      df <- merge(data, dplyr::select(B, -site), by = c("featureid"), 
                  all.x = T)
      for (i in 2:length(names(B))) {
        df[, names(B[i])][is.na(df[, names(B[i])])] <- colMeans(B[i])
      }
    }
  }
  else {
    B[, var.name] <- as.character(B[, var.name])
    data <- as.data.frame(unclass(data))
    data[, var.name] <- as.character(data[, var.name])
    df <- merge(data, B, by = var.name, all.x = T)
    for (i in 2:length(names(B))) {
      df[, names(B[i])][is.na(df[, names(B[i])])] <- colMeans(B[i])
    }
  }
  return(df)
}

predictTemp <- function (data, data.fit = tempDataSyncS, coef.list, cov.list, 
                         featureid_site) 
{
  B.site <- prepConditionalCoef(coef.list = coef.list, cov.list = cov.list, 
                                var.name = "site")
  B.huc <- prepConditionalCoef(coef.list = coef.list, cov.list = cov.list, 
                               var.name = "huc")
  B.year <- prepConditionalCoef(coef.list = coef.list, cov.list = cov.list, 
                                var.name = "year")
  B.ar1 <- prepConditionalCoef(coef.list = coef.list, cov.list = cov.list, 
                               var.name = "ar1")
  df <- prepPredictDF(data = data, coef.list = coef.list, cov.list = cov.list, 
                      var.name = "site", featureid_site = featureid_site)
  df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, 
                      var.name = "huc", featureid_site = featureid_site)
  df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, 
                      var.name = "year", featureid_site = featureid_site)
  df <- prepPredictDF(data = df, coef.list = coef.list, cov.list = cov.list, 
                      var.name = "ar1", featureid_site = featureid_site)
  df$trend <- NA
  df$trend <- as.vector(coef.list$B.fixed$mean %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), 
                                                                                            one_of(cov.list$fixed.ef))))))) + rowSums(as.matrix(select(df, 
                                                                                                                                                       one_of(cov.list$site.ef))) * as.matrix(select(df, one_of(names(B.site[-1]))))) + 
    rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * 
              as.matrix(select(df, one_of(names(B.huc[-1]))))) + 
    rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * 
              as.matrix(select(df, one_of(names(B.year[-1])))))
  df <- mutate(df, prev.temp = c(NA, temp[(2:(nrow(data))) - 
                                            1]))
  df <- mutate(df, prev.trend = c(NA, trend[(2:nrow(data)) - 
                                              1]))
  df <- mutate(df, prev.err = prev.temp - prev.trend)
  df <- mutate(df, tempPredicted = trend)
  df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), 
                                                       ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), 
                                                                                                             ]$prev.err
  return(df)
}





system.time(ggs.ar1 <- ggs(M.ar1)) 

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
saveRDS(coef.list, file=output_file)


rm(list = ls())
gc()

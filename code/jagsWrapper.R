#' @title modelRegionalTempAR1
#'
#' @description
#' \code{modelRegionalTempAR1} Linear mixed model in JAGS to model daily stream temperature
#'
#' @param data dataframe created in the 3-statModelPrep.Rmd script
#' @param data.fixed Dataframe of fixed effects parameter names from columns in data. These are parameters without random slopes.
#' @param data.random.sites Dataframe of variables to have random slopes by site
#' @param data.random.years Dataframe of variables to have random slopes by year
#' @param deployments Named vector of the logger deployment starting positions (rows) within the dataframe.
#' @param params Character string of parameters to monitor (return) from the model
#' @param n.burn Integer number of iterations in the burn-in (adapation) phase of the MCMC
#' @param n.it Integer number of iterations per chain to run after the burn-in
#' @param n.thin Integer: save every nth iteration
#' @param n.chains Integer number of chains. One run per cluster so should use <= # cores on computer
#' @param codaTF Logical if TRUE return coda mcmc.list, if FALSE (default) return jags.samples object
#' 
#' @return Returns the iterations from the Gibbs sampler for each variable in params as either an mcmc.list or jags.samples object
#' @details
#' This function takes daily observed stream temperatures, air temperature, day of the year, and landscape covariates for a linear mixed effects model with site within HUC8 and year random effects.
#' 
#' @examples
#' 
#' \dontrun{
#' M.ar <- modelRegionalTempAR(data, data.fixed, data.random.sites, data.random.years, n.burn = 1000, n.it = 1000, n.thin = 1, nc = 3, coda = coda.tf, param.list = monitor.params)
#' }
#' @export
modelRegionalTempAR1 <- function(data = tempDataSyncS, cov.list, formulae = NULL, firstObsRows, evalRows, param.list, n.burn = 5000, n.it = 3000, n.thin = 3, nc = 3, codaTF = FALSE, runParallel = TRUE) {
  #  temp.model <- function(){
{
  sink("code/modelRegionalTempAR1.txt")
  cat("
      model{
      # Likelihood
      for(i in 1:nFirstObsRows) {
      trend[firstObsRows[i]] <- inprod(B.0[], X.0[firstObsRows[i], ]) + 
      inprod(B.site[site[firstObsRows[i]], ], X.site[firstObsRows[i], ]) + 
      inprod(B.huc[huc[firstObsRows[i]], ], X.site[firstObsRows[i], ]) + 
      inprod(B.year[year[firstObsRows[i]], ], X.year[firstObsRows[i], ])
      
      stream.mu[firstObsRows[i]] <- trend[firstObsRows[i]]
      }
      # restart counter for each deployment
      for(i in 1:nEvalRows) {
      trend[evalRows[i]] <- inprod(B.0[], X.0[evalRows[i], ]) + 
      inprod(B.site[site[evalRows[i]], ], X.site[evalRows[i], ]) + 
      inprod(B.huc[huc[evalRows[i]], ], X.site[evalRows[i], ]) + 
      inprod(B.year[year[evalRows[i]], ], X.year[evalRows[i], ])
      
      stream.mu[evalRows[i]] <- trend[evalRows[i]] + B.ar1[site[evalRows[i]]] * (temp[evalRows[i]-1] - trend[evalRows[i]-1])
      }
      
      for(i in 1:n) {
      temp[i] ~ dnorm(stream.mu[i], tau) # T(0, 50) - truncation causes MCMC problem: no mixing/movement
      residuals[i] <- temp[i] - stream.mu[i]
      }
      
      # Prior for autoregressive
      #B.ar1 ~ dunif(-1, 1)
      for(j in 1:J){ # J sites
      B.ar1[j] ~ dnorm(mu.ar1, tau.ar1)T(-1, 1)
      }
      mu.ar1 ~ dunif(-1, 1)
      sigma.ar1 ~ dunif(0, 10)
      tau.ar1 <- pow(sigma.ar1, -2)
      
      # prior for model variance
      sigma ~ dunif(0, 100)
      tau <- pow(sigma, -2)
      
      for(k in 1:K.0){
      B.0[k] ~ dnorm(0, 0.0001) # priors coefs for fixed effect predictors
      }
      
      # SITE Effects
      # Independent priors on random site effects
      for(k in 1:K) {
      sigma.b.site[k] ~ dunif(0, 100)
      tau.b.site[k] <- 1 / (sigma.b.site[k] * sigma.b.site[k])
      for(j in 1:J){ # J sites
      B.site[j, k] ~ dnorm(0, tau.b.site[k])
      }
      }
      
      # HUC Effects
      # Priors for random effects of huc
      for(m in 1:M){ # M hucs
      B.huc[m, 1:K] ~ dmnorm(mu.huc[ ], tau.B.huc[ , ])
      }
      mu.huc[1] <- 0 # this would not be necessary if I didn't include an overall intercept term. The only difference is whether correlation is allowed between parameters and the intercept (currently not).
      for(k in 2:K){
      mu.huc[k] ~ dnorm(0, 0.0001)
      }
      
      # Prior on multivariate normal std deviation
      tau.B.huc[1:K, 1:K] ~ dwish(W.huc[ , ], df.huc)
      df.huc <- K + 1
      sigma.B.huc[1:K, 1:K] <- inverse(tau.B.huc[ , ])
      for(k in 1:K){
      for(k.prime in 1:K){
      rho.B.huc[k, k.prime] <- sigma.B.huc[k, k.prime]/sqrt(sigma.B.huc[k, k]*sigma.B.huc[k.prime, k.prime])
      }
      sigma.b.huc[k] <- sqrt(sigma.B.huc[k, k])
      }
      
      # YEAR EFFECTS
      # Priors for random effects of year
      for(t in 1:Ti){ # Ti years
      B.year[t, 1:L] ~ dmnorm(mu.year[ ], tau.B.year[ , ])
      }
      mu.year[1] <- 0
      for(l in 2:L){
      mu.year[l] ~ dnorm(0, 0.0001)
      }
      
      # Prior on multivariate normal std deviation
      tau.B.year[1:L, 1:L] ~ dwish(W.year[ , ], df.year)
      df.year <- L + 1
      sigma.B.year[1:L, 1:L] <- inverse(tau.B.year[ , ])
      for(l in 1:L){
      for(l.prime in 1:L){
      rho.B.year[l, l.prime] <- sigma.B.year[l, l.prime]/sqrt(sigma.B.year[l, l]*sigma.B.year[l.prime, l.prime])
      }
      sigma.b.year[l] <- sqrt(sigma.B.year[l, l])
      }
      
      # Derived parameters
      
      }
      ",fill = TRUE)
  sink()
} # sink needs to be wrapped in expression for knitr to work

# Fixed effects
#library(dplyr)

data.cal <- prepDF(data, covars = cov.list)

X.0 <- data.cal$data.fixed
variables.fixed <- names(X.0)
K.0 <- length(variables.fixed)


# Random site effects
X.site <- data.cal$data.random.sites
variables.site <- names(X.site)
J <- length(unique(data$site))
K <- length(variables.site)
n <- dim(data)[1]
W.site <- diag(K)

M <- length(unique(data$HUC8))
W.huc <- diag(K)

# Random Year effects
X.year <- data.cal$data.random.years
variables.year <- names(X.year)
Ti <- length(unique(data$year))
L <- length(variables.year)
W.year <- diag(L)

data.list <- list(n = n, 
                  J = J, 
                  K = K, 
                  Ti = Ti,
                  L = L,
                  M = M,
                  K.0 = K.0,
                  X.0 = as.matrix(X.0), # X.0
                  W.site = W.site,
                  W.year = W.year,
                  W.huc = W.huc,
                  temp = data$temp,
                  evalRows = evalRows,
                  nEvalRows = length(evalRows),
                  firstObsRows = firstObsRows,
                  nFirstObsRows = length(firstObsRows),
                  X.site = as.matrix(X.site), # X.site, #
                  X.year = as.matrix(X.year),
                  site = as.factor(data$site),
                  huc = as.factor(data$HUC8),
                  year = as.factor(data$year))

inits <- function(){
  list(#B.raw = array(rnorm(J*K), c(J,K)), 
    #mu.site.raw = rnorm(K),
    sigma = runif(1))
  #tau.B.site.raw = rwish(K + 1, diag(K)),)
}

if(class(param.list) == "character") {
  params <- param.list
} else {
  params <- c("sigma",
              "B.ar1",
              "B.0",
              "B.site",
             # "rho.B.site",
              # "mu.site",
              "sigma.b.site",
              "B.huc",
              "rho.B.huc",
              "mu.huc",
              "sigma.b.huc",
              "B.year",
              "rho.B.year",
              "mu.year",
              "sigma.b.year",
              "residuals")#,
  # "stream.mu")
}

#M1 <- bugs(data, )

# n.burn = 5000
#  n.it = 3000
# n.thin = 3

library(parallel)
library(rjags)

if(nc) {
  nc <- nc
} else {
  nc <- 3 
}

# model.tc <- textConnection(modelstring)
#filename <- file.path(tempdir(), "tempmodel.txt")
#write.model(temp.model, filename)

if(runParallel) {
  if(codaTF) {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempAR1.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(as.mcmc(fm))
    }))
    
    M3 <- mcmc.list(out)
    
    stopCluster(CL)
    return(M3)
    
  } else {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempAR1.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(fm)
    }))
    
    stopCluster(CL)
    return(out)
  }
  
  
} else {
  if(coda) {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempAR1.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  } else {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempAR1.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  }
}
}



#' @title modelRegionalTempHUC
#'
#' @description
#' \code{modelRegionalTempHUC} Linear mixed model in JAGS to model daily stream temperature
#'
#' @param data dataframe created in the 3-statModelPrep.Rmd script
#' @param data.fixed Dataframe of fixed effects parameter names from columns in data. These are parameters without random slopes.
#' @param data.random.sites Dataframe of variables to have random slopes by site
#' @param data.random.years Dataframe of variables to have random slopes by year
#' @param params Character string of parameters to monitor (return) from the model
#' @param n.burn Integer number of iterations in the burn-in (adapation) phase of the MCMC
#' @param n.it Integer number of iterations per chain to run after the burn-in
#' @param n.thin Integer: save every nth iteration
#' @param n.chains Integer number of chains. One run per cluster so should use <= # cores on computer
#' @param coda Logical if TRUE return coda mcmc.list, if FALSE (default) return jags.samples object
#' 
#' @return Returns the iterations from the Gibbs sampler for each variable in params as either an mcmc.list or jags.samples object
#' @details
#' This function takes daily observed stream temperatures, air temperature, day of the year, and landscape covariates for a linear mixed effects model with site within HUC8 and year random effects.
#' 
#' @examples
#' 
#' \dontrun{
#' M.huc <- modelRegionalTempHUC(data, data.fixed, data.random.sites, data.random.years, n.burn = 1000, n.it = 1000, n.thin = 1, nc = 3, coda = coda.tf, param.list = monitor.params)
#' }
#' @export
modelRegionalTempHUC <- function(data = tempDataSyncS, data.fixed, data.random.sites, data.random.years, param.list, n.burn = 5000, n.it = 3000, n.thin = 3, nc = 3, coda = FALSE, runParallel = TRUE) {
  #  temp.model <- function(){
{
  sink("code/modelRegionalTempHUC.txt")
  cat("
      model{
      # Likelihood
      for(i in 1:n){ # n observations
      temp[i] ~ dnorm(stream.mu[i], tau)
      stream.mu[i] <- inprod(B.0[], X.0[i, ]) + inprod(B.site[site[i], ], X.site[i, ]) + inprod(B.huc[huc[i], ], X.site[i, ]) + inprod(B.year[year[i], ], X.year[i, ]) #  
      }
      
      # prior for model variance
      sigma ~ dunif(0, 100)
      tau <- pow(sigma, -2)
      
      for(k in 1:K.0){
      B.0[k] ~ dnorm(0, 0.001) # priors coefs for fixed effect predictors
      }
      
      # SITE Effects
      # Independent priors on random site effects
      for(k in 1:K) {
      sigma.b.site[k] ~ dunif(0, 100)
      tau.b.site[k] <- 1 / (sigma.b.site[k] * sigma.b.site[k])
      for(j in 1:J){ # J sites
      B.site[j, k] ~ dnorm(0, tau.b.site[k])
      }
      }
      
      # HUC Effects
      # Priors for random effects of huc
      for(m in 1:M){ # M hucs
      B.huc[m, 1:K] ~ dmnorm(mu.huc[ ], tau.B.huc[ , ])
      }
      mu.huc[1] <- 0
      for(k in 2:K){
      mu.huc[k] ~ dnorm(0, 0.0001)
      }
      
      # Prior on multivariate normal std deviation
      tau.B.huc[1:K, 1:K] ~ dwish(W.huc[ , ], df.huc)
      df.huc <- K + 1
      sigma.B.huc[1:K, 1:K] <- inverse(tau.B.huc[ , ])
      for(k in 1:K){
      for(k.prime in 1:K){
      rho.B.huc[k, k.prime] <- sigma.B.huc[k, k.prime]/sqrt(sigma.B.huc[k, k]*sigma.B.huc[k.prime, k.prime])
      }
      sigma.b.huc[k] <- sqrt(sigma.B.huc[k, k])
      }
      
      # YEAR EFFECTS
      # Priors for random effects of year
      for(t in 1:Ti){ # Ti years
      B.year[t, 1:L] ~ dmnorm(mu.year[ ], tau.B.year[ , ])
      }
      mu.year[1] <- 0
      for(l in 2:L){
      mu.year[l] ~ dnorm(0, 0.0001)
      }
      
      # Prior on multivariate normal std deviation
      tau.B.year[1:L, 1:L] ~ dwish(W.year[ , ], df.year)
      df.year <- L + 1
      sigma.B.year[1:L, 1:L] <- inverse(tau.B.year[ , ])
      for(l in 1:L){
      for(l.prime in 1:L){
      rho.B.year[l, l.prime] <- sigma.B.year[l, l.prime]/sqrt(sigma.B.year[l, l]*sigma.B.year[l.prime, l.prime])
      }
      sigma.b.year[l] <- sqrt(sigma.B.year[l, l])
      }
      
      # Derived parameters
      for(i in 1:n) {
      residuals[i] <- temp[i] - stream.mu[i]
      }
      }
      ",fill = TRUE)
  sink()
} # sink needs to be wrapped in expression for knitr to work

# Fixed effects
#library(dplyr)

X.0 <- data.fixed
variables.fixed <- names(X.0)
K.0 <- length(variables.fixed)


# Random site effects
#variables.site <- c("Intercept-site",
#                   "Air Temperature",
#                  "Air Temp Lag1",
#                 "Air Temp Lag2",
#                "Precip",
#               "Precip Lag1",
#              "Precip Lag2")

# Slope, Aspect, Dams/Impoundments, Agriculture, Wetland, Coarseness, dayl, srad, swe

X.site <- data.random.sites
variables.site <- names(X.site)
J <- length(unique(data$site))
K <- length(variables.site)
n <- dim(data)[1]
W.site <- diag(K)

M <- length(unique(data$HUC8))
W.huc <- diag(K)

# Random Year effects
#variables.year <- c("Intercept-year",
#                  "dOY",
#                 "dOY2",
#                "dOY3")

X.year <- data.random.years
variables.year <- names(X.year)
Ti <- length(unique(data$year))
L <- length(variables.year)
W.year <- diag(L)

data.list <- list(n = n, 
                  J = J, 
                  K = K, 
                  Ti = Ti,
                  L = L,
                  M = M,
                  K.0 = K.0,
                  X.0 = X.0,
                  W.site = W.site,
                  W.year = W.year,
                  W.huc = W.huc,
                  temp = data$temp,
                  X.site = X.site, #as.matrix(X.site),
                  X.year = as.matrix(X.year),
                  site = as.factor(data$site),
                  huc = as.factor(data$HUC8),
                  year = as.factor(data$year))

inits <- function(){
  list(#B.raw = array(rnorm(J*K), c(J,K)), 
    #mu.site.raw = rnorm(K),
    sigma = runif(1))
  #tau.B.site.raw = rwish(K + 1, diag(K)),)
}

if(class(param.list) == "character") {
  params <- param.list
} else {
  params <- c("sigma",
              "B.0",
              "B.site",
              "rho.B.site",
              # "mu.site",
              "sigma.b.site",
              "B.huc",
              "rho.B.huc",
              "mu.huc",
              "sigma.b.huc",
              "B.year",
              "rho.B.year",
              "mu.year",
              "sigma.b.year",
              "residuals")#,
  # "stream.mu")
}

#M1 <- bugs(data, )

# n.burn = 5000
#  n.it = 3000
# n.thin = 3

library(parallel)
library(rjags)

if(nc) {
  nc <- nc
} else {
  nc <- 3 
}

# model.tc <- textConnection(modelstring)
#filename <- file.path(tempdir(), "tempmodel.txt")
#write.model(temp.model, filename)

if(runParallel) {
  if(coda) {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempHUC.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(as.mcmc(fm))
    }))
    
    M3 <- mcmc.list(out)
    
    stopCluster(CL)
    return(M3)
    
  } else {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempHUC.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(fm)
    }))
    
    stopCluster(CL)
    return(out)
  }
  
  
} else {
  if(coda) {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempHUC.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  } else {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempHUC.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  }
}
}

#' @title modelRegionalTempWB
#'
#' @description
#' \code{modelRegionalTempWB} Linear mixed model in JAGS to model daily stream temperature in the West Brook and it's tributaries
#'
#' @param data dataframe created in the 3-statModelPrep.Rmd script
#' @param data.fixed Dataframe of fixed effects parameter names from columns in data. These are parameters without random slopes.
#' @param data.random.years Dataframe of variables to have random slopes by year
#' @param params Character string of parameters to monitor (return) from the model
#' @param n.burn Integer number of iterations in the burn-in (adapation) phase of the MCMC
#' @param n.it Integer number of iterations per chain to run after the burn-in
#' @param n.thin Integer: save every nth iteration
#' @param n.chains Integer number of chains. One run per cluster so should use <= # cores on computer
#' @param coda Logical if TRUE return coda mcmc.list, if FALSE (default) return jags.samples object
#' 
#' @return Returns the iterations from the Gibbs sampler for each variable in params as either an mcmc.list or jags.samples object
#' @details
#' This function takes daily observed stream temperatures, air temperature, day of the year, and landscape covariates for a linear mixed effects model with site within HUC8 and year random effects.
#' 
#' @examples
#' 
#' \dontrun{
#' M.wb <- modelRegionalTempWB(data=DF, firstObsRows, evalRows, data.fixed = DF.fixed, data.random.years = DF.years, n.burn = 1000, n.it = 1000, n.thin = 1, nc = 3, coda = coda.tf, param.list = monitor.params)
#' }
#' @export

modelRegionalTempWB <- function(data = tempDataSyncS, firstObsRows, evalRows, data.fixed, data.random.years, param.list, n.burn = 5000, n.it = 3000, n.thin = 3, nc = 3, coda = FALSE, runParallel = TRUE) {
  #  temp.model <- function(){
{
  sink("code/modelRegionalTempWB.txt")
  cat("
      model{
      #       # Likelihood
      #       for(i in 1:n){ # n observations
      #         temp[i] ~ dnorm(stream.mu[i], tau)
      #         stream.mu[i] <- inprod(B.0[], X.0[i, ]) + inprod(B.year[year[i], ], X.year[i, ]) #  
      #       }
      
      # Likelihood w ar1
      for(i in 1:nFirstObsRows){      
      temp[firstObsRows[i]] ~ dnorm(stream.mu[firstObsRows[i]], tau) 
      stream.mu[firstObsRows[i]] <- trend[firstObsRows[i]]
      trend[firstObsRows[i]] <- inprod(B.0[], X.0[firstObsRows[i], ]) + inprod(B.year[year[firstObsRows[i]], ], X.year[firstObsRows[i], ])
      }
      
      for(i in 1:nEvalRows){ # n observations
      temp[evalRows[i]] ~ dnorm(stream.mu[evalRows[i]], tau)
      #        stream.mu[evalRows[i]] <- trend[ evalRows[i] ] + ar1 * (temp[evalRows[i]-1] - trend[ evalRows[i]-1 ]) 
      stream.mu[evalRows[i]] <- trend[ evalRows[i] ] + ar1[river[ evalRows[i] ]] * (temp[evalRows[i]-1] - trend[ evalRows[i]-1 ])
      trend[ evalRows[i] ]  <- inprod(B.0[], X.0[evalRows[i], ]) + inprod(B.year[year[evalRows[i]], ], X.year[evalRows[i], ])
      }
      
      for( i in 1:nRiver ){
      ar1[i] ~ dnorm( ar1Mean, pow(ar1SD,-2) ) T(-1,1)       
      #        ar1 ~ dunif(-1,1)
      #ar1 ~ dunif(-0.001,0.001) #turn off ar1
      }
      
      ar1Mean ~ dunif( -1,1 ) 
      ar1SD ~ dunif( 0, 2 )
      
      # prior for model variance
      sigma ~ dunif(0, 100)
      tau <- pow(sigma, -2)
      
      for(k in 1:K.0){
      B.0[k] ~ dnorm(0, 0.001) # priors coefs for fixed effect predictors
      }
      
      # YEAR EFFECTS
      # Priors for random effects of year
      for(t in 1:Ti){ # Ti years
      B.year[t, 1:L] ~ dmnorm(mu.year[ ], tau.B.year[ , ])
      }
      mu.year[1] <- 0
      for(l in 2:L){
      mu.year[l] ~ dnorm(0, 0.0001)
      }
      
      # Prior on multivariate normal std deviation
      tau.B.year[1:L, 1:L] ~ dwish(W.year[ , ], df.year)
      df.year <- L + 1
      sigma.B.year[1:L, 1:L] <- inverse(tau.B.year[ , ])
      for(l in 1:L){
      for(l.prime in 1:L){
      rho.B.year[l, l.prime] <- sigma.B.year[l, l.prime]/sqrt(sigma.B.year[l, l]*sigma.B.year[l.prime, l.prime])
      }
      sigma.b.year[l] <- sqrt(sigma.B.year[l, l])
      }
      
      # Derived parameters
      residuals[1] <- 0 # hold the place. Not sure if this is necessary...
      for(i in 2:n) {
      residuals[i] <- temp[i] - stream.mu[i]
      }
      }
      ",fill = TRUE)
  sink()
} # sink needs to be wrapped in expression for knitr to work

X.0 <- data.fixed
variables.fixed <- names(X.0)
K.0 <- length(variables.fixed)

n <- dim(data)[1]

X.year <- data.random.years
variables.year <- names(X.year)
Ti <- length(unique(data$year))
L <- length(variables.year)
W.year <- diag(L)

data.list <- list(n = n,  
                  Ti = Ti,
                  L = L,
                  K.0 = K.0,
                  X.0 = X.0,
                  W.year = W.year,
                  temp = data$temp,
                  X.year = as.matrix(X.year),
                  site = as.factor(data$site),
                  river = as.factor(data$riverOrdered),
                  year = as.factor(data$year),
                  dOYInt = data$dOYInt,
                  nSite = length(unique(data$site)),
                  nRiver = length(unique(data$riverOrdered)),
                  evalRows = evalRows,
                  firstObsRows = firstObsRows,
                  nEvalRows = length(evalRows),
                  nFirstObsRows = length(firstObsRows)
)

inits <- function(){
  list(#B.raw = array(rnorm(J*K), c(J,K)), 
    #mu.site.raw = rnorm(K),
    sigma = runif(1))
  #tau.B.site.raw = rwish(K + 1, diag(K)),)
}

if(class(param.list) == "character") {
  params <- param.list
} else {
  params <- c("sigma",
              "B.0",
              "B.huc",
              "B.year",
              "rho.B.year",
              "mu.year",
              "sigma.b.year",
              "residuals")#,
  # "stream.mu")
}

#M1 <- bugs(data, )

# n.burn = 5000
#  n.it = 3000
# n.thin = 3

library(parallel)
library(rjags)

if(nc) {
  nc <- nc
} else {
  nc <- 3 
}

# model.tc <- textConnection(modelstring)
#filename <- file.path(tempdir(), "tempmodel.txt")
#write.model(temp.model, filename)

if(runParallel) {
  if(coda) {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "Ti", "L", "n", "W.year", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempWB.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(as.mcmc(fm))
    }))
    
    M3 <- mcmc.list(out)
    
    stopCluster(CL)
    return(M3)
    
  } else {
    CL <- makeCluster(nc)
    clusterExport(cl=CL, list("data.list", "inits", "params", "Ti", "L", "n", "W.year", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
    clusterSetRNGStream(cl=CL, iseed = 2345642)
    
    system.time(out <- clusterEvalQ(CL, {
      library(rjags)
      load.module('glm')
      jm <- jags.model("code/modelRegionalTempWB.txt", data.list, inits, n.adapt = n.burn, n.chains=1)
      fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(fm)
    }))
    
    stopCluster(CL)
    return(out)
  }
  
  
} else {
  if(coda) {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempWB.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  } else {
    library(rjags)
    load.module('glm')
    jm <- jags.model("code/modelRegionalTempWB.txt", data.list, inits, n.adapt = n.burn, n.chains=nc)
    fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
    return(fm)
  }
}
}


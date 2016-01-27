#' @title modelRegionalTempAR1Scaled
#'
#' @description
#' \code{modelRegionalTempAR1Scaled} Linear mixed model in JAGS to model daily stream temperature
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
#' @param coda Logical if TRUE return coda mcmc.list, if FALSE (default) return jags.samples object
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
modelRegionalTempAR1Scaled <- function(data = tempDataSyncS, cov.list, formulae = NULL, firstObsRows, evalRows, param.list, n.burn = 5000, n.it = 3000, n.thin = 3, nc = 3, coda = FALSE, runParallel = TRUE, cluster_type = NULL, data_dir = data_dir) {
  #  temp.model <- function(){
  {
    sink(paste0(data_dir, "/modelRegionalTempAR1.txt"))
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
        
        stream.mu[evalRows[i]] <- trend[evalRows[i]] + B.ar1 * (temp[evalRows[i]-1] - trend[evalRows[i]-1])
        }
        
        for(i in 1:n) {
        temp[i] ~ dnorm(stream.mu[i], tau) # T(0, 50) - truncation causes MCMC problem: no mixing/movement
        residuals[i] <- temp[i] - stream.mu[i]
        }
        
        # Prior for autoregressive
        B.ar1 ~ dunif(-1, 1)
#         for(j in 1:J){ # J sites
#         B.ar1[j] ~ dnorm(mu.ar1, tau.ar1)T(-1, 1)
#         }
#         mu.ar1 ~ dunif(-1, 1)
#         sigma.ar1 ~ dunif(0, 10)
#         tau.ar1 <- pow(sigma.ar1, -2)
        
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
        B.huc.raw[m, 1:K] ~ dmnorm(mu.huc.raw[1:K], tau.B.huc.raw[ , ])
        }
        mu.huc.raw[1] <- 0 # this would not be necessary if I didn't include an overall intercept term. The only difference is whether correlation is allowed between parameters and the intercept (currently not).
        #mu.huc[1] <- xi.huc[1] * mu.huc.raw[1]
        #xi.huc[1] <- 0
        xi.huc[1] ~ dunif(0, 100)
        for(k in 2:K){
        mu.huc.raw[k] ~ dnorm(0, 0.0001)
        mu.huc[k] <- xi.huc[k] * mu.huc.raw[k]
        xi.huc[k] ~ dunif(0, 100)
        }
        
        for(m in 1:M) {
        for(k in 1:K) {
        B.huc[m, k] <- xi.huc[k] * B.huc.raw[m, k]
        }
        }
        
        # Prior on multivariate normal std deviation
        tau.B.huc.raw[1:K, 1:K] ~ dwish(W.huc[ , ], df.huc)
        df.huc <- K + 1
        sigma.B.huc.raw[1:K, 1:K] <- inverse(tau.B.huc.raw[ , ])
        for(k in 1:K){
        for(k.prime in 1:K){
        rho.B.huc[k, k.prime] <- sigma.B.huc.raw[k, k.prime]/sqrt(sigma.B.huc.raw[k, k]*sigma.B.huc.raw[k.prime, k.prime])
        }
        }
        
        #sigma.b.huc[1] <- sqrt(sigma.B.huc.raw[1, 1])
        for(k in 1:K) {
        sigma.b.huc[k] <- abs(xi.huc[k]) * sqrt(sigma.B.huc.raw[k, k])
        }
        
        # YEAR EFFECTS
        # Priors for random effects of year
        for(t in 1:Ti){ # Ti years
        B.year.raw[t, 1:L] ~ dmnorm(mu.year.raw[1:L], tau.B.year.raw[ , ])
        }
        mu.year.raw[1] <- 0 # this would not be necessary if I didn't include an overall intercept term. The only difference is whether correlation is allowed between parameters and the intercept (currently not).
        mu.year[1] <- xi.year[1] * mu.year.raw[1]
        # xi.year[1] <- 0
        xi.year[1] ~ dunif(0, 100)
        for(l in 2:L){
        mu.year.raw[l] ~ dnorm(0, 0.0001)
        mu.year[l] <- xi.year[l] * mu.year.raw[l]
        xi.year[l] ~ dunif(0, 100)
        }
        
        for(t in 1:Ti) {
        for(l in 1:L) {
        B.year[t, l] <- xi.year[l] * B.year.raw[t, l]
        }
        }
        
        # Prior on multivariate normal std deviation
        tau.B.year.raw[1:L, 1:L] ~ dwish(W.year[ , ], df.year)
        df.year <- L + 1
        sigma.B.year.raw[1:L, 1:L] <- inverse(tau.B.year.raw[ , ])
        for(l in 1:L){
        for(l.prime in 1:L){
        rho.B.year[l, l.prime] <- sigma.B.year.raw[l, l.prime]/sqrt(sigma.B.year.raw[l, l]*sigma.B.year.raw[l.prime, l.prime])
        }
        }
        
        #sigma.b.year[1] <- sqrt(sigma.B.year.raw[1, 1])
        for(l in 1:L) {
        sigma.b.year[l] <- abs(xi.year[l]) * sqrt(sigma.B.year.raw[l, l])
        }
        
        # Derived parameters
        reduced.pred[1:500] <- stream.mu[1:500]
        reduced.trend[1:500] <- trend[1:500]
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
  sites <- data$sitef
  J <- length(unique(sites))
  K <- length(variables.site)
  n <- dim(data)[1]
  W.site <- diag(K)
  
  hucs <- data$hucf
  M <- length(unique(hucs))
  W.huc <- diag(K)
  
  # Random Year effects
  X.year <- data.cal$data.random.years
  variables.year <- names(X.year)
  years <- data$yearf
  Ti <- length(unique(years))
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
                    site = sites,
                    huc = hucs,
                    year = years
  )
  
  tau.huc <- as.matrix(cbind(
    c(runif(1,3.5,4.5), runif(1,-2.5, -2), runif(1,-3,-2.5)), 
    c(0, runif(1,10, 20), runif(1,-0.5,-0.1)), 
    c(0, 0, runif(1,3,10))))
  
  tau.year = as.matrix(cbind(
    c(runif(1,5,6.2), runif(1,-2, -1.5), runif(1,1,1.5), runif(1,-2.5,-2)), 
    c(0, runif(1,3, 5.5), runif(1,0,0.5), runif(1,3,4)), 
    c(0, 0, runif(1,5,10), runif(1,0.5,1.2)),
    c(0, 0, 0, runif(1,3,5.5))))
  
  inits <- function(L = L, K = K, tau.huc = tau.huc, tau.year = tau.year){
    list(#B.raw = array(rnorm(J*K), c(J,K)), 
      #mu.site.raw = rnorm(K),
      #tau.B.huc.raw = tau.huc + t(tau.huc) - diag(diag(tau.huc)),
      #tau.B.year.raw = tau.year + t(tau.year) - diag(diag(tau.year)),
      xi.year = c(runif(1, 0.2, 0.4), runif(1, 0.2, 0.4), runif(1,0.1,0.3), runif(1,0.01, 0.1)),
      xi.huc = c(runif(1, 1.8, 2.5), runif(1, 0.9, 1.1), runif(1, 0.7, 0.9)),
      sigma = runif(1)
    )
  }
  
  if(class(param.list) == "character") {
    params <- param.list
  } else {
    params <- c("sigma",
                "B.ar1",
                "B.0",
                "B.site",
                #"rho.B.site",
                # "mu.site",
                "stream.mu",
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
    if(coda) { # run in parallel and use coda output
      if(is.null(cluster_type)) {
        switch(Sys.info()[['sysname']],
               Windows = {
                 print("I'm sorry, I'm a Windows PC. I will run in parallel using makeSockCluster.")
                 cluster_type = "PSOCK"
               },
               Linux  = {
                 print("I'm a penguin. I will efficiently run in parallel using makeForkCluster.")
                 cluster_type = "FORK"
               },
               Darwin = {
                 print("I'm a Mac, lucky you. I will efficiently run in parallel using makeForkCluster.")
                 cluster_type = "FORK"
               }
        )
      } else {
        if(!(cluster_type %in% c("PSOCK", "FORK"))) {
          switch(Sys.info()[['sysname']],
                 Windows = {
                   print("I'm sorry, I'm a Windows PC. I will run in parallel using makeSockCluster.")
                   cluster_type = "PSOCK"
                 },
                 Linux  = {
                   print("I'm a penguin. I will efficiently run in parallel using makeForkCluster.")
                   cluster_type = "FORK"
                 },
                 Darwin = {
                   print("I'm a Mac, lucky you. I will efficiently run in parallel using makeForkCluster.")
                   cluster_type = "FORK"
                 }
          )
        }
      }
      CL <- makeCluster(nc, type = cluster_type)
      clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin", "data_dir", "tau.year", "tau.huc"), envir = environment())
      clusterSetRNGStream(cl=CL, iseed = 2345642)
      
      system.time(out <- clusterEvalQ(CL, {
        library(rjags)
        load.module('glm')
        jm <- jags.model(paste0(data_dir, "/modelRegionalTempAR1.txt"), data.list, inits, n.adapt = n.burn, n.chains=1)
        fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
        return(as.mcmc(fm))
      }))
      
      M3 <- mcmc.list(out)
      
      stopCluster(CL)
      return(M3)
      
    } else { # run in parallel but use rjags output
      switch(Sys.info()[['sysname']],
             Windows = {
               print("I'm sorry, I'm a Windows PC. I will run in parallel using makeSockCluster.")
               cluster_type = "PSOCK"
             },
             Linux  = {
               print("I'm a penguin. I will efficiently run in parallel using makeForkCluster.")
               cluster_type = "FORK"
             },
             Darwin = {
               print("I'm a Mac, lucky you. I will efficiently run in parallel using makeForkCluster.")
               cluster_type = "FORK"
             }
      )
      CL <- makeCluster(nc, type = cluster_type)
      clusterExport(cl=CL, list("data.list", "inits", "params", "K", "J", "Ti", "L", "n", "W.site", "W.huc", "M", "W.year", "X.site", "X.year", "n.burn", "n.it", "n.thin"), envir = environment())
      clusterSetRNGStream(cl=CL, iseed = 2345642)
      
      system.time(out <- clusterEvalQ(CL, {
        library(rjags)
        load.module('glm')
        jm <- jags.model(paste0(data_dir, "/modelRegionalTempAR1.txt"), data.list, inits, n.adapt = n.burn, n.chains=1)
        fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
        return(fm)
      }))
      
      stopCluster(CL)
      return(out)
    }
    
    
  } else { # if don't want to run in parallel
    if(coda) {
      library(rjags)
      load.module('glm')
      jm <- jags.model(paste0(data_dir, "/modelRegionalTempAR1.txt"), data.list, inits, n.adapt = n.burn, n.chains=nc)
      fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(fm)
    } else {
      library(rjags)
      load.module('glm')
      jm <- jags.model(paste0(data_dir, "/modelRegionalTempAR1.txt"), data.list, inits, n.adapt = n.burn, n.chains=nc)
      fm <- jags.samples(jm, params, n.iter = n.it, thin = n.thin)
      return(fm)
    }
  }
  }


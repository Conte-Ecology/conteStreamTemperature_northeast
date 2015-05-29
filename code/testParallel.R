library(parallel)

a = 3
b = 4
c = runif(10, 1, 10)

CL <- makeCluster(3)
clusterExport(cl=CL, list("a", "b", "c"), envir = environment())
clusterSetRNGStream(cl=CL, iseed = 2345642)

system.time(out <- clusterEvalQ(CL, {
  ab <- a * b
  ac <- a * c
  return(ab)
}))

M3 <- mcmc.list(out)

stopCluster(CL)
return(M3)

}
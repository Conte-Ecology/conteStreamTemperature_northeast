# test speed of abind vs list to array

n.iter <- 3000
n.row <- 100
n.col <- 20
X <- rnorm(n.row*n.col)

library(abind)
mat1 <- matrix(X, n.row, n.col)*1
mat.array <- mat1
start <- Sys.time()
for(i in 2:n.iter) {
  mat2 <- matrix(X, n.row, n.col)*i
  mat.array <- abind(mat.array, mat2, along = 3)
}
end <- Sys.time()
end-start # 4 seconds


mat.list <- list()
start2 <- Sys.time()
for(i in 1:n.iter) {
  mat3 <- matrix(X, n.row, n.col)*i
  mat.list[[i]] <- mat3
}
mat.array2 <- array(unlist(mat.list), dim = c(nrow(mat3), ncol(mat3), n.iter))
end2 <- Sys.time()
end2-start2 # 0.3 seconds

# check that they resulting arrays match
mean1 <- apply(mat.array, 1:2, mean, na.rm = TRUE)
mean2 <- apply(mat.array2, 1:2, mean, na.rm = TRUE)

all(mean1 == mean2)

str(mat.array)
str(mat.array2)

rm(list = c("mat1", "mat2", "mat.array", "mat.array2", "mat.list", "end", "start", "start2", "end2"))
gc()

n <- 10

D <- matrix(100*dnorm(1:10), ncol=1)

X <- Matrix::Matrix(0, nrow = n, ncol = n, sparse = T)

for (i in 1:n) {
  for (j in 1:n) {
    X[i,j] <- max(D[i,1], D[j,1])
  }
}

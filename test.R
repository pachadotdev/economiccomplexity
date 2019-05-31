set.seed(10)
n <- 100
D <- runif(n,0,100)

X <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  for (j in 1:n) {
    X[i,j] <- max(D[i], D[j])
  }
}

Y <- pairwise_max(n,D)

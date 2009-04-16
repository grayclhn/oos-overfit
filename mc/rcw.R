rcw <- function(n, v = 1, edist = rnorm) {
### Generates dataframe of matrix of n observations y_t, z_t
### from Clark and West's (2007) DGP 1
### v is a scalar that multiplies the covariance matrix for debugging 

  burn <- 3500
  n <- n + burn

  vcv <- v * matrix(c(sqrt(10.505), 0, 1.036 / sqrt(10.505),
                      sqrt(0.366 - 1.036^2 / 10.505)), 2, 2)
  
  z <- rep(NA,n)
  y <- rep(NA,n)
  y[1:4] <- 0
  z[1:4] <- 0

  innovations <- crossprod(vcv, matrix(edist(n * 2), 2, n))
  for (s in 5:n) {
    y[s] <- 2.237 + 0.261 * y[s-1] + innovations[1,s]
    z[s] <- 0.804 * z[s-1] - 0.221 * z[s-2] + 0.226 * z[s-3] - 0.205 * z[s-4] +
      innovations[2, s]
  }
  data.frame(y = y[-(1:burn)], z = z[-(1:burn)])
}

cw.eloss <- function(coef) {
  F <- matrix(0, 10, 10)
  F[1,1] <- 0.261
  F[2:5,1:4] <- diag(4)
  F[6,6] <- 0.804
  F[6,7] <- -0.221
  F[6,8] <- 0.226
  F[6,9] <- -0.205
  F[7:10,6:9] <- diag(4)

  mu <- 2.237 / (1 - 0.261)
  
  Q <- matrix(0, 10, 10)
  Q[c(1,6),c(1,6)] <- c(10.505, 1.036, 1.036, 0.366)
  S <- matrix(solve(diag(100) - kronecker(F, F), c(Q)), 10, 10)
  c(1, - coef[2:5], 0, -coef[6:9]) %*% S %*% c(1, - coef[2:5], 0, -coef[6:9]) +
    (coef[1] - (1 - sum(coef[2:5])) * mu)^2
}

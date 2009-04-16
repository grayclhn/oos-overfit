source("../R/cw_prederrors.R")
source("../R/rcw.R")
library(sandwich)

rvec <- c(80, 120)
pvec <- c(40, 80, 120, 160)

n <- 1
n <- 3000
maxn <- max(rvec) + max(pvec)
minr <- min(rvec)

teststat <- matrix(NA, n, length(rvec) * length(pvec))
muR <- matrix(NA, n, length(rvec))

k <- 0
for (i in 1:n) {
  d.i <- cw.prederrors(rcw(maxn), R = minr)^2 %*% c(1, -1)
  for (r in rvec) {
    for (p in pvec) {
      stat.irp <- lm(d.i ~ 1, subset = r - minr + 1:p)
      nw.irp <- NeweyWest(stat.irp)
    teststat[i,k] <- stat.ik$coef / nw.ik
  }
  print(paste("done simulation", i, "of", n))
}clear

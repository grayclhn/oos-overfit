source("../R/cw_prederrors.R")
source("../R/rcw.R")
library(sandwich)

rvec <- c(80, 120)
pvec <- c(40, 80, 120, 160)
tvec <- unique(c(outer(rvec, pvec, "+")))

n <- 10
maxn <- max(rvec) + max(pvec)
minr <- min(rvec)

teststat <- matrix(NA, n * length(rvec) * length(pvec))

j <- 0
k <- 0
i <- 1
mur <- rep(0, length(tvec))

while (i <= n) {
  d.i <- cw.prederrors(rcw(maxn), R = minr, Tvec = tvec)
  for (tt in tvec) {
    
  for (r in rvec) {
    ##    mur[j <- j+1] <- 
    for (p in pvec) {
      stat.irp <- lm(d.i ~ 1, subset = r - minr + 1:p)
      teststat[k <- k+1] <- stat.irp$coef / NeweyWest(stat.irp)
    }
  }
  print(paste("done simulation", i, "of", n))
}

results <- data.frame(t1 = teststat > 1.282
                      t2 = teststat)

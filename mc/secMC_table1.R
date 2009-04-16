library(sandwich)
source("../R/cw_prederrors.R")
source("../R/rcw.R")

edist <- function(n) {
  e1 <- rnorm(n, -1)
  e2 <- rnorm(n, 2, 4)
  r <- ifelse(rbinom(n, size = 1, p = 0.5), e1, e2)
  r <- (r - 1/2) / sqrt(11)
}

mc.T <- c(100, 200, 300)
mc.ratio <- c(1/3, 1/2, 2/3, 4/5)

n <- 1000
teststat <- matrix(NA, n, length(mc.T) * (1 + length(mc.ratio)))
minR <- round(min(mc.T) * min(mc.ratio))
for (i in 1:n) {
  data.i <- rcw(max(mc.T), edist = edist)
  d.i <- c(cw.prederrors(data.i, R = round(min(mc.ratio) *  min(mc.T)))^2 %*% c(1,-1))
  k <- 0
  for (nobs in mc.T) {
    s2 <- 5:nobs
    m1 <- lm(data.i$y[s2] ~ data.i$y[s2 - 1])
    m2 <- lm(data.i$y[s2] ~ data.i$y[s2-1] + data.i$y[s2-2] + data.i$y[s2-3] + data.i$y[s2-4]
             + data.i$z[s2-1] + data.i$z[s2-2] + data.i$z[s2-3] + data.i$z[s2-4])
    teststat[i, k <- k+1] <- (anova(m1, m2)$Pr[2] < 0.10)
    for (ratio in mc.ratio) {
      d.subset <- (round(ratio * nobs) - minR) + 1:(nobs - round(ratio * nobs))
      avgdiff <- lm(d.i[d.subset] ~ 1)
      teststat[i, k <- k+1] <- avgdiff$coef / NeweyWest(avgdiff)[1,1] > 1.282
    }
  }
  print(paste("done simulation", i, "of", n))
}

tabby <- matrix(colMeans(teststat), length(mc.T), byrow = T)

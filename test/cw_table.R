source("../R/cw_prederrors.R")
source("../R/rcw.R")


### test that cw.prederrors can reproduce Clark and West's (2007) Table 1
### - for DGP 2 recursive regressions.

cw.table <- matrix(byrow = TRUE, c( 80, 40, 0.024,  80, 80, 0.010,  80, 120, 0.006,  80, 160, 0.002,
                                   120, 40, 0.030, 120, 80, 0.013, 120, 120, 0.008, 120, 160, 0.006), 8, 3)
nstats <- dim(cw.table)[1]

n <- 3000
maxn <- max(rowSums(cw.table[,1:2]))
minR <- min(cw.table[,1])
teststat <- matrix(NA, n, dim(cw.table)[1])
for (i in 1:n) {
  d.i <- as.matrix(cw.prederrors(rcw(maxn), R = minR)^2) %*% c(1, -1)
  for (k in 1:nstats) {
    R <- cw.table[k,1]
    P <- cw.table[k,2]
    teststat[i,k] <- (sqrt(P) * mean(d.i[(R - minR) + 1:P]) / sd(d.i[(R - minR) + 1:P])) > 1.282
  }
  print(paste("done simulation", i, "of", n))
}

for (k in 1:nstats) {
  bt <- binom.test(x = sum(teststat[,k]), n, p = cw.table[k,3],
                   alternative = "t")
  if (bt$p.value < 0.001) 
    stop("Monte Carlo inconsistent with Clark and West --- R ", cw.table[k,1],
         " P ", cw.table[k,2], " CI = ", signif(bt$conf[1], 2), " to ",
         signif(bt$conf[2], 2), " but should be ", cw.table[k,3])
}

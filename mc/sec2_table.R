source("../R/cw_prederrors.R")
source("../R/rcw.R")

cw.table <- matrix(byrow = TRUE, c(80, 40, NA,  80, 80, NA,  80, 120, NA,
                     80, 160, NA, 120, 40, NA, 120, 80, NA, 120, 120, NA,
                     120,160, NA), 8, 3)
nstats <- dim(cw.table)[1]

n <- 3000
maxn <- max(rowSums(cw.table[,1:2]))
minR <- min(cw.table[,1])
teststat <- matrix(NA, n, dim(cw.table)[1])
for (i in 1:n) {
  d.i <- cw.prederrors(rcw(maxn), R = minR)[,2]^2
  for (k in 1:nstats) {
    R <- cw.table[k,1]
    P <- cw.table[k,2]
    teststat[i,k] <- (sqrt(P) * (mean(d.i[(R - minR) + 1:P]) - 10.505)
                      / sd(d.i[(R - minR) + 1:P])) > 1.282
  }
  print(paste("done simulation", i, "of", n))
}

cw.table[,3] <- colMeans(teststat)

library(sandwich)
library(lattice)
source("rcw.R")
source("cw_eloss.R")
source("rTestStat.R")

trPairs <- matrix(c(120,  40, 
                    120,  80,
                    240,  80,
                    240, 160,
                    240, 200,
                    480, 160,
                    480, 320,
                    480, 400, 
                    480, 440), 
                  ncol = 2, byrow = TRUE, dimnames = list(NULL, c("T","R")))
n <- 1000
maxT <- max(trPairs[,1])

rStatistic <- tStatistic <- rep(NA, n * dim(trPairs)[1])

k <- 0
for (i in 1:n) {
  data.i <- rcw(maxT)
  for (j in 1:dim(trPairs)[1]) {
    R <- trPairs[j,2]
    P <- trPairs[j,1] - R
    k <- k+1
    
    testStats.ij  <- rTestStat(d = data.i[seq(to = maxT, length = R + P),], R, P)
    rStatistic[k]  <- testStats.ij$mR
    tStatistic[k]  <- testStats.ij$mT
  }
  print(i)
}

mcResults <- data.frame(rbind(trPairs, trPairs))
mcResults$P <- as.factor(mcResults$T - mcResults$R)
mcResults$T <- as.factor(paste("T =", mcResults$T))
mcResults$R <- as.factor(mcResults$R)
mcResults$stat <- as.factor(rep(c("R", "T"), each = dim(trPairs)[1]))
mcResults$size <- 100 * c(rowMeans(matrix(abs(rStatistic)  > 1.645, ncol = n)),
                          rowMeans(matrix(abs(tStatistic)  > 1.645, ncol = n)))

png("mcTable.png", width = 720, height = 360)
dotplot(data = mcResults, P ~ (100 - size) | T, ylab = "P", xlab = "Empirical Coverage",
        groups = stat, pch = 19, col = c("black", "red"), layout = c(3,1),
        scales = list(alternating = 1, axs = "i"), xlim = c(70, 100))
dev.off()

save.image(file = "mcData_redone.RData")

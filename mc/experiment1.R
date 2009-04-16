library(lattice)
rm(list = ls())

rootVCV <- matrix(c(sqrt(10.505), 0, 1.036 / sqrt(10.505),
sqrt(0.366 - 1.036^2 / 10.505)), 2, 2)
B <- matrix(c(2.237, 0.261, 3.363, 0.000,-0.633, 0.000,-0.377, 0.000,-0.529,
              0.000, 0.000, 0.804, 0.000,-0.221, 0.000, 0.226, 0.000,-0.205), 
            byrow = T, nrow = 2)

set.seed(5343)
source("mcFunctions.R")
n <- c(80, 120, 240, 480)
Rratio <- c(1/4, 1/2, 3/4, 9/10)

minwindow <- 20
lossFrame <- cbind(expand.grid(R = paste(Rratio,"n"), sim = 1:(nsims <- 400), n = n),
                   mFull1 = NA, mFull2 = NA, mTrain1 = NA, mTrain2 = NA)

for (j in seq(along.with = n)) {
  for (i in 1:nsims) {
    cat(n[j], "--", i, "of", nsims, "\n")
    d.ni <- rData1(n[j], B, rootVCV)
    lossFrame[(j-1) * nsims * length(Rratio) + (i-1) * length(Rratio) + 1:length(Rratio),
              c("mFull1", "mFull2")] <- rep(marginalLoss(makeb1(d.ni), makeb2(d.ni), B, rootVCV),
                                            each = length(Rratio))
    for (r in seq(along.with = Rratio)) {
      lossFrame[(j-1) * nsims * length(Rratio) + (i-1) * length(Rratio) + r,
                c("mTrain1", "mTrain2")] <- marginalLoss(makeb1(d.ni[1:round(Rratio[r] * n[j]),]),
                                                         makeb2(d.ni[1:round(Rratio[r] * n[j]),]),
                                                         B, rootVCV)
    }
  }
}

lims1 <- c(2.3, 3)
xyplot(log(mFull1) ~ log(mTrain1) | n * Rratio, data = lossFrame,
       xlim = lims1, ylim = lims1, aspect = "fill", pch = 19, cex = 0.1)

histogram(~ log(mFull1) - log(mTrain1) | n * Rratio, data = lossFrame, nint = 180,
          xlim = c(-0.5, 0.1))

lims2 <- c(2.63, 2.8)
xyplot(log(mFull2) ~ log(mTrain2) | n * Rratio, data = lossFrame,
       xlim = lims2, ylim = lims2, aspect = "fill", pch = 19, cex = 0.1)

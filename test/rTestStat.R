source("../mc/rTestStat.R")
source("../mc/rcw.R")
source("../mc/cw_eloss.R")
library(sandwich)

testvec <- rTestStat(R = 50, P = 100)
if (!is.list(testvec))
  stop("rTestStat does not return a vector.")

if (!is.numeric(testvec$avg))
  stop("rTestStat does not return an average loss.")
if (!is.numeric(testvec$mR))
  stop("rTestStat does not return mR.")
if (!is.numeric(testvec$mT))
  stop("rTestStat does not return mT.")
if (!is.numeric(testvec$CE))
  stop("rTestStat does not return CE.")

testvec2 <- testvec1 <- rep(NA, 400)
for (i in 1:400) {
  testvec1[i] <- rTestStat(100, 1000)$mR
  testvec2[i] <- rTestStat(10000, 80)$mT
}

if (shapiro.test(testvec1)$p.value < 0.01)
  stop("(R) failed normality test at 1%")
if (t.test(testvec1)$p.value < 0.01)
  stop("(R)failed t-test that mean is zero at 1%")

if (shapiro.test(testvec2)$p.value < 0.01)
  stop("(T) failed normality test at 1%")
if (t.test(testvec2)$p.value < 0.01)
  stop("(T)failed t-test that mean is zero at 1%")


### Runs simulation to generate QQ plots --- examine whether out-of-sample averages are normal.

library(lattice)
rm(list = ls())

rootVCV <- matrix(c(sqrt(10.505), 0, 1.036 / sqrt(10.505),
                    sqrt(0.366 - 1.036^2 / 10.505)), 2, 2)
B <- matrix(c(2.237, 0.261, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
              0.000, 0.000, 0.804, 0.000,-0.221, 0.000, 0.226, 0.000,-0.205), 
            byrow = T, nrow = 2)

set.seed(5343)
source("mcFunctions.R")
n <- c(80, 120, 240, 480)
Rratio <- c(1/4, 1/2, 3/4, 9/10)

calStat <- cbind(expand.grid(R = paste(Rratio,"n"), sim = 1:(nsims <- 400), n = n), rvT = NA, rvR = NA, recT = NA)

for (j in seq(along.with = n)) {
  Rvec.j <- round(Rratio * n[j])
  for (i in 1:nsims) {
    cat(n[j], "-", i, "of", nsims, "\n")
    d.ji <- rData1(n[j], B, rootVCV)
    
    normalStats.ji <- calPop(Rvec.j, d.ji, B, rootVCV)
    expanding.ji <- recPop(Rvec.j, d.ji, B, rootVCV)

    calStat[(j-1) * nsims * length(Rvec.j) + (i-1) * length(Rvec.j) + 1:length(Rvec.j), "rvT"] <- normalStats.ji[,"T"]
    calStat[(j-1) * nsims * length(Rvec.j) + (i-1) * length(Rvec.j) + 1:length(Rvec.j), "rvR"] <- normalStats.ji[,"R"]

    calStat[(j-1) * nsims * length(Rvec.j) + (i-1) * length(Rvec.j) + 1:length(Rvec.j), "recT"] <- expanding.ji[,"T"]
    calStat[(j-1) * nsims * length(Rvec.j) + (i-1) * length(Rvec.j) + 1:length(Rvec.j), "recR"] <- expanding.ji[,"R"]
  }
}

qqmath( ~ rvR | n + R, data = calStat, type = "l")
qqmath( ~ rvT | n + R, data = calStat, type = "l")
histogram( ~ rvR | n + R, data = calStat,nint =  90)
histogram( ~ rvT | n + R, data = calStat, nint = 90)

calTrim <- calStat[as.character(calStat$R) != "0.9 n",]

qqmath( ~ recR | n + R, data = calStat, type = "l")
qqmath( ~ recT | n + R, data = calStat, type = "l")
histogram( ~ recR | n + R, data = calTrim, nint =  90)
histogram( ~ recT | n + R, data = calTrim, nint = 90, col = 'black')


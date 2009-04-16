### Simulation to calculate probability of choosing worse-forecasting larger model.

### initialize
rm(list = ls())
set.seed(5700)
library(lattice)
library(fixedWindow)

### set coefficient values to use for simulation
rootVCV <- matrix(c(sqrt(10.505), 0, 1.036 / sqrt(10.505),
                    sqrt(0.366 - 1.036^2 / 10.505)), 2, 2)
B <- matrix(c(2.237, 0.261, 3.363, 0.000,-0.633, 0.000,-0.377, 0.000,-0.529,
              0.000, 0.000, 0.804, 0.000,-0.221, 0.000, 0.226, 0.000,-0.205), 
            byrow = T, nrow = 2)

levelvec <- c(0.90, 0.95, 0.99)
## column 1 - R values
## column 2 - P values
windowmat <- matrix(c(50,  50, 100, 100, 100, 200, 200, 200,
                      25,  50,  25,  50, 100,  25,  50, 100), ncol=2)
evalwindow <- 30
ndraw <- evalwindow + max(rowSums(windowmat))

realPerformance <- rep(NA, (nsims <- 1500) * dim(windowmat)[1])
ciCal <- ciCW <- ciMc <-
  matrix(NA, length(realPerformance), length(levelvec))

for (i in 1:nsims) {
  cat("Simulation", i, "of", nsims, "\n")
  ## draw a random dataset
  d.i <- rData1(ndraw, B, rootVCV)
  ## loop over values of R and P
  for (j in 1:dim(windowmat)[1]) {
    ij <- j + (i-1) * dim(windowmat)[1]

    R.ij <- windowmat[j,1]
    P.ij <- windowmat[j,2]

    preds0.ij <- forecast0(R.ij, P.ij, d.i)
    predsA.ij <- forecastA(R.ij, P.ij, d.i)
    target.ij <- d.i[1:P.ij + R.ij,1]

    realPerformance[ij] <- mean((d.i[R.ij+P.ij + 1:evalwindow,1] -
                                 forecast0(R.ij+P.ij, evalwindow, d.i))^2 -
                                (d.i[R.ij+P.ij + 1:evalwindow,1] -
                                 forecastA(R.ij+P.ij, evalwindow, d.i))^2)

    ## loop over confidence levels
    for (k in seq(along.with = levelvec)) {
      
      ## calculate my confidence interval
      ciCal[ij,k] <- calhounCI(preds0.ij, predsA.ij, target.ij, levelvec[k])
      ## calculate clark and west confidence intervals
      ciCW[ij,k] <- clarkwestCI(preds0.ij, predsA.ij, target.ij, levelvec[k])
      ## calculate clark and mccracken for 90, 95, and 99
      ciMc[ij,k] <- mccrackenCI(preds0.ij, predsA.ij, target.ij,
                                levelvec[k], 4, P.ij/R.ij)
    }
  }
}

## compare average out-of-sample loss to confidence intervals
cbind(windowmat, aggregate(realPerformance > ciCal,
                           by = list(win = rep(1:dim(windowmat)[1], nsims)),
                           mean))

cbind(windowmat, aggregate(realPerformance > ciCW,
                           by = list(win = rep(1:dim(windowmat)[1], nsims)),
                           mean))

cbind(windowmat, aggregate(realPerformance > ciMc,
                           by = list(win = rep(1:dim(windowmat)[1], nsims)),
                           mean))

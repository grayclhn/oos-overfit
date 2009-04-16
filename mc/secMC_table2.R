source("../R/cw_prederrors.R")
source("../R/cw_eloss.R")


i1 <- 2:maxn
i2 <- 5:maxn

for (i in 1:n) {

  dataMatrix1.i$dep <- data.i$y[i1]
  dataMatrix2.i$dep <- data.i$y[i2]
  dataMatrix1.i$ylag <- data.i$y[i1-1]
  for (j in 1:4) {
    dataMatrix2.i[,paste("y",j, sep="")] <- data.i$y[i2 - j]
    dataMatrix2.i[,paste("z",j, sep="")] <- data.i$z[i2 - j]
  }
  for (k in 1:nstats) {
    R <- cw.table[k,1]
    P <- cw.table[k,2]

    lastPredictors.i <- c(1, as.matrix(dataMatrix2.i[R+P-3,-1]))
    model1.ik <- lm(dep ~ ylag, data = dataMatrix1.i, subset = 1:(R-1))
    model2.ik <- lm(dep ~ y1 + y2 + y3 + y4 + z1 + z2 + z3 + z4,
                    data = dataMatrix2.i, subset = 1:(R-4))
 
    fullCoef1.ik <- c(lm(dep ~ ylag, data = dataMatrix1.i, subset = 1:(R+P-1))$coef, rep(0, 7))
    fullCoef2.ik <- lm(dep ~ y1 + y2 + y3 + y4 + z1 + z2 + z3 + z4,
                       data = dataMatrix2.i, subset = 1:(R+P-4))$coef
    
    diffCE.ik <- (lastPredictors.i %*% (fullCoef1.ik - pseudoTrue))^2 - 
      (lastPredictors.i %*% (fullCoef2.ik - pseudoTrue))^2
    diffFull.ik <- cw.eloss(fullCoef1.ik) - cw.eloss(fullCoef2.ik)
    diffSubsample.ik <- 

    loss.ik <- (dataMatrix1.i$dep[R-1 + 1:P] - predict(model1.ik, dataMatrix1.i[R-1 + 1:P,]))^2 -
      (dataMatrix2.i$dep[R-4 + 1:P] - predict(model2.ik, dataMatrix2.i[R-1 + 1:P,]))^2
    
    avg.ik <- lm(loss.ik ~ 1)

    cw.table[k,"Full"] <- cw.table[k,"Full"] + ((abs((avg.ik$coef - diffFull.ik) / std.ik) < 1.645) / n)

    cw.table[k,"CE"]   <- cw.table[k,"CE"]   + ((abs((avg.ik$coef - diffCE.ik) / std.ik) < 1.645) / n)
  }
  print(paste("done simulation", i, "of", n))
}

rTestStat <- function(d = rcw(P+R), R, P) {
  ret <- list(avg = NA, mR = NA, mT = NA, CE = NA)
  
  r1 <- 1:(R-1)
  r2 <- 1:(R-4)
  t1 <- R+P - 1
  t2 <- R+P - 4
  
  predictors1 <- cbind(1, d$y[1:t1])
  predictors2 <- cbind(1, matrix(d$y[outer(1:t2, 3:0, "+")], t2, 4),
                       matrix(d$z[outer(1:t2, 3:0, "+")], t2, 4))

  model1 <- lm.fit(y = d$y[r1 + 1], x = predictors1[r1,])
  model2 <- lm.fit(y = d$y[r2 + 4], x = predictors2[r2,])
  model1Full <- lm.fit(y = d$y[1:t1 + 1], x = predictors1)
  model2Full <- lm.fit(y = d$y[1:t2 + 4], x = predictors2)

  lossDiff = (d$y[seq(length = P, to = R+P)] - predictors1[-r1,] %*% model1$coef)^2 -
    (d$y[seq(length = P, to = R+P)] - predictors2[-r2,] %*% model2$coef)^2

  avgLoss <- lm(lossDiff ~ 1)
  stdAvg <- sqrt(NeweyWest(avgLoss)[1,1])
  
  ## studentized value around period-R models
  ret$avg <- as.numeric(avgLoss$coef)
  ret$std <- as.numeric(stdAvg)
  ret$mR <- (avgLoss$coef - (cw.eloss(c(model1$coef, rep(0,7))) - cw.eloss(model2$coef))) / stdAvg
  ret$mT <- (avgLoss$coef - (cw.eloss(c(model1Full$coef, rep(0,7))) - cw.eloss(model2$coef))) / stdAvg
  ret$CE <- (avgLoss$coef - ((predictors1[t1,] %*% (model1Full$coef - c(2.237, 0.261)))^2 -
                             (predictors2[t2,] %*% (model2Full$coef - c(2.237, 0.261, rep(0,7))))^2)) / stdAvg
  ret
}

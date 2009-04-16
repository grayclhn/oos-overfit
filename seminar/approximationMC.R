load("mcGlobal.Rdata")

set.seed(30)

mc <- rep(NA, nsims)
integralMC <- rmc(n = 100000)

for (i in 1:nsims) {
  d.i <- rmc()
  
  model <- lm(y ~ poly(x, K), data = d.i, subset = estimationWindow)
  err <- d.i$y[-estimationWindow] - predict(model, data.frame(x = d.i$x[-estimationWindow]))

  mc[i] <- sqrt(100) * (mean(err^2) - mean((integralMC$y - predict(model, integralMC))^2)) / sd(err^2)
  print(i)
}
## change

save.image("approximation.Rdata")

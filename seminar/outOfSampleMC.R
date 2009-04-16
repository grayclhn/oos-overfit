load("mcGlobal.Rdata")

set.seed(30)

mcQuad    <- rep(NA, nsims)
mcOverfit <- rep(NA, nsims)

for (i in 1:nsims) {
  d.i <- rmc()
  
  modelQuad    <- lm(y ~ poly(x, 2), data = d.i, subset = estimationWindow)
  modelOverfit <- lm(y ~ poly(x, K), data = d.i, subset = estimationWindow)

  errQuad    <- d.i$y[-estimationWindow] - predict(modelQuad,    data.frame(x = d.i$x[-estimationWindow]))
  errOverfit <- d.i$y[-estimationWindow] - predict(modelOverfit, data.frame(x = d.i$x[-estimationWindow]))

  mcQuad[i]    <- mean(errQuad^2)
  mcOverfit[i] <- mean(errOverfit^2)

  print(i)
}

save.image("simulations.Rdata")

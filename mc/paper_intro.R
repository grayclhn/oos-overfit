r2 <- function(n){
  y <- arima.sim(model = list(ar = 0.1), n + 4, n.start = 500)
  x <- arima.sim(model = list(ar = 0.3), n + 4, n.start = 500)
  data.frame(y = y[4 + 1:n], y1 = y[3 + 1:n], y2 = y[2 + 1:n], y3 = y[1 + 1:n], y4 = y[1:n],
             x = x[4 + 1:n], x1 = x[3 + 1:n], x2 = x[2 + 1:n], x3 = x[1 + 1:n], x4 = x[1:n])
}

makeerrors <- function(dataset, k, R) {
  P <- length(dataset$y) - R
  varmodel <- switch(k,
                     lm(data = dataset, subset = 1:R, y ~ 1),
                     lm(data = dataset, subset = 1:R, y ~ y1),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2 + x2),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2 + x2 + y3),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2 + x2 + y3 + x3),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2 + x2 + y3 + x3 + y4),
                     lm(data = dataset, subset = 1:R, y ~ y1 + x1 + y2 + x2 + y3 + x3 + y4 + x4))
  dataset[R + 1:P, "y"] - predict(varmodel, newdata = dataset[R + 1:P,])
}

R <- 4 * 20
P <- 4 * 30
nsims <- 1000
ncoefs <- 9
pmse <- rep(NA, nsims * (ncoefs - 1))
stat.i <- 0
for (s in 1:nsims) {
  vardata.s <- r2(R + P)
  for (k in 2:9) {
    stat.i <- stat.i + 1
    pmse[stat.i] <- mean(makeerrors(vardata.s, k, R)^2)
  }
}
mcresults <- data.frame(pmse = pmse, zstat = (pmse - 1) / (2 / sqrt(P)), k = rep(2:9))

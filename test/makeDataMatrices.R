source("../empirics/makeDataMatrices.R")

y <- 1:30
x <- 301:330

r <- makeDataMatrices(y,x)
if (!(isTRUE(all.equal(r$target, 25:30))))
  stop("incorrect target")
if (!(isTRUE(all.equal(r$predictorMatrix, cbind(1, outer(1:6,12:1,"+"), outer(301:306,12:1,"+"))))))
  stop("incorrect predictor matrix")

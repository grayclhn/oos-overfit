GenerateOosForecast <- function(X.full, Y, ntrain, nreg, b.true, allR = FALSE) {
  nreg.full <- length(b.true)
  nreg.excess <- nreg.full - nreg
  stopifnot(nreg.full == ncol(X.full))

  if (allR) {
    ntrain <- seq.int(min(ntrain), length(Y) - 1, 1)
  } else {
    ntrain <- sort.int(ntrain)
  }

  X <- X.full[, 1:nreg, drop = FALSE]
  PaddedEstimates <- function(coef) c(coef, rep.int(0, nreg.excess))

  ## Generate fixed-window and recursive-window forecasts
  pred.list <- lapply(ntrain, function(R) {
    coef <- lm.fit(X[1:R, ], Y[1:R])$coefficients
    list(coef = PaddedEstimates(coef),
         pred = drop(tcrossprod(coef, X[-(1:R),, drop=FALSE])))
  })
  new("oos.forecast",
      ## Expected loss associated with the estimates using only the
      ## test sample
      expected.loss.test = sapply(pred.list, function(p) EL(p$coef, b.true)),
      ## Expected loss associated with the full-sample estimates
      expected.loss.future = EL(PaddedEstimates(lm.fit(X, Y)$coefficients), b.true),
      ## Fixed-window forecasts
      forecasts = lapply(pred.list, function(p) p$pred),
      target = Y[-(1:ntrain[1])],
      nreg = as.integer(nreg))
}

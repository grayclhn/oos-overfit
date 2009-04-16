source("../R/cw_eloss.R")

if (! isTRUE(all.equal(cw.eloss(c(2.237, 0.261, rep(0, 7))), 10.505)))
  stop("Does not return correct value at true parameters")

if (! isTRUE(all.equal(optim(c(2.237, 0.261, rep(0, 7)),
                             cw.eloss,
                             control = list(maxit = 10000),
                             method = "SANN")$value, 10.505)))
  stop("Optimization does not return correct value at true parameters")


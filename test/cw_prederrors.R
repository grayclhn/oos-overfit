source("../R/cw_prederrors.R")
source("../R/rcw.R")
n <- 50
R <- 30

### check that west_prederror returns the right number of prediction errors.

terr <- cw.prederrors(rcw(n = n), R)
if (! is.data.frame(terr))
  stop("cw.prederrors does not return a dta frame")
if (! isTRUE(all.equal(dim(terr), c(n - R, 2))))
  stop("cw.prederrors returns the wrong size data frame")

### check that the forecast errors are plausible
if (isTRUE(all.equal(terr, rep(0, 2 * (n - R)), check.arguments = FALSE)))
  stop("cw.prederrors should NOT return zero forecast error.")

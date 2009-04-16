source("../mc/rcw.R")
n <- 30

### first check that it returns the right number of series and observations

td <- rcw(n)
if (! is.data.frame(td))
  stop("rcw does not return a data frame.")
if (! isTRUE(all.equal(dim(td), c(n, 2)))) {
  stop("rcw does not return the correct dimension")
}


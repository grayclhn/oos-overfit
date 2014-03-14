library(sandwich)
library(lmtest)
library(Hmisc)
load("data/empirical-results.RData")

devnull <-
  latex(coeftest(fullmod, vcov=NeweyWest(fullmod, prewhite=FALSE, lag=2)),
        file = "floats/empirics-coeftest.tex", digits = 2)

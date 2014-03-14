library(sandwich)
library(lmtest)
library(Hmisc)
load("data/empirical-results.RData")

devnull <-
  latex(waldtest(fullmod, vcov=NeweyWest(fullmod, prewhite=FALSE, lag=2), test="F"),
        file = "floats/empirics-waldtest.tex", digits = 2,
        caption = "Wald test for the null hypothesis that all of the coefficients
                   in Goyal and Welch's (2008) ``kitchen sink'' model are zero,
                   except for the intercept.",
        label = "tab:gwinsample")


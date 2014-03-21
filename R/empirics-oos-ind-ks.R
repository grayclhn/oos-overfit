library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

indplot(name = "Full model", oos[,"KS"],
        file = "floats/empirics-oos-ind-ks.tex")

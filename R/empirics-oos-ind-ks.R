library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

indplot(name = "KS", oos[,"KS"],
        file = "floats/empirics-oos-ind-ks.tex")

library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

indplot(name = "PM", oos[,"PM"],
        file = "floats/empirics-oos-ind-pm.tex", xlabel = "$R$",
        ticks = c(0, .01, .02, .03, .04, 0.05),
        labels = c("0", "", "", "", "", "0.05"))

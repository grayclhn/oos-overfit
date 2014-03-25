library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

indplot(name = "Benchmark", oos[,"PM"],
        file = "floats/empirics-oos-ind-pm.tex",
        xlabel = "Observations used for training sample ($R$)",
        ticks = c(0, .01, .02, .03, .04, 0.05),
        labels = c("0", "", "", "", "", "0.05"))

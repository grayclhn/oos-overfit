library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

oos.subset <- oos.ct
window(oos.subset, end = 49) <- NA

# Subset of Average OOS MSE Difference for Restricted OLS Equity
# Premium Forecasts
oosplot(oos.subset[,c("interval", "average")],
        file = "floats/empirics-oos-mse-2b.tex",
        xlabel = "$R$",
        ticks = c(0, - 0.015/2, -.015, -(0.015 + 0.03)/2, -.03),
        labels = c("0", "", "", "", "-.03"))

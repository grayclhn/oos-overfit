library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

oos.subset <- oos.ct
window(oos.subset, end = 49) <- NA

## Average OOS MSE Difference for Restricted OLS Equity Premium
## Forecasts
oosplot(oos.ct[,c("interval", "average")],
        file = "floats/empirics-oos-mse-2.tex",
        ticks = c(0, -1, -2, -3, -4),
        labels = c("0", "", "-2", "", "-4"))

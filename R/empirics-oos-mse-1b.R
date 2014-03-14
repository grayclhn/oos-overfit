library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

oos.subset <- oos
window(oos.subset, end = 49) <- NA
## Subset of Average OOS MSE Difference\nfor OLS Equity Premium
## Forecasts
oosplot(oos.subset[,c("interval", "average")],
        file = "floats/empirics-oos-mse-1b.tex",
        xlabel = "$R$", ticks = c(0, -0.025, -.05, -0.075, -.1),
        labels = c("0", "", "-0.5", "", "-.10"))


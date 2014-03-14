library(sandwich)
library(lmtest)
library(Hmisc)
library(tikzDevice)
load("data/empirical-results.RData")

##Average OOS MSE Difference\nfor OLS Equity Premium Forecasts
oosplot(oos[,c("interval", "average")],
        file = "floats/empirics-oos-mse-1.tex",
        ticks = c(0, -1, -2, -3, -4),
        labels = c("0", "", "-2", "", "-4"))

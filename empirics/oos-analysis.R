library(lattice)
library(sandwich)
library(lmtest)
library(Hmisc)

dfull <- ts(read.csv("goyal-code/AllData2009.csv")[,-1],
            start = 1871, frequency = 1)
stock.returns <- (dfull[,"price"] + dfull[,"dividend"]) / lag(dfull[,"price"], -1) - 1
startyear <- 1928

## make a ts object with the variables we want to use in the analysis
d <- lag(cbind(equity.premium              = lag(log1p(stock.returns)
                 - log1p(dfull[,"risk.free.rate"]), 1),
               default.yield.spread        = dfull[,"baa.rate"] - dfull[,"aaa.rate"],
               inflation                   = dfull[,"inflation"],
               stock.variance              = dfull[,"stock.variance"],
               dividend.payout.ratio       = log(dfull[,"dividend"]) - log(dfull[,"earnings"]),
               long.term.yield             = dfull[,"long.term.yield"],
               term.spread                 = dfull[,"long.term.yield"] - dfull[,"treasury.bill"],
               treasury.bill               = dfull[,"treasury.bill"],
               default.return.spread       = dfull[,"corporate.bond.returns"] - dfull[,"long.term.rate"],
               dividend.price.ratio        = log(dfull[,"dividend"]) - log(dfull[,"price"]),
               dividend.yield              = log(dfull[,"dividend"]) - log(lag(dfull[,"price"], -1)),
               long.term.rate              = dfull[,"long.term.rate"],
               earnings.price.ratio        = log(dfull[,"earnings"]) - log(dfull[,"price"]),
               book.to.market.ratio        = dfull[,"book.to.market.ratio"],
               investment.to.capital.ratio = dfull[,"investment.to.capital.ratio"],
               net.equity.expansion        = dfull[,"net.equity.expansion"],
               percent.equity.issuing      = dfull[,"percent.equity.issuing"]),
         k=-1)

fullmodel <- (equity.premium ~ dividend.price.ratio + earnings.price.ratio + stock.variance +
              book.to.market.ratio + net.equity.expansion + percent.equity.issuing +
              treasury.bill + long.term.yield + long.term.rate + default.return.spread +
              default.yield.spread + inflation)
## drop variables whose persistance and/or breaks destroys their
## economic relationship with equity premium (this is for my own
## knowledge and debugging, mostly).
## fullmodel <- update.formula(fullmodel, . ~ . - book.to.market.ratio - treasury.bill - long.term.yield)

forecasts <- function(x, newdata, nonneg,...) {
  f <- predict(x, newdata, ...)
  if (nonneg)
    return(pmax(0, f))
  else
    return(f)
}

oosT <- function(R, mod.formula, dframe, nonneg) {
  f1 <- forecasts(lm(equity.premium ~ 1, data = dframe[1:R,]),
                  dframe[-(1:R),], nonneg)
  f2 <- forecasts(lm(mod.formula, data = dframe[1:R,]),
                  dframe[-(1:R),], nonneg)
  target <- dframe$equity.premium[-(1:R)]
  
  loss1 <- (target - f1)^2
  loss2 <- (target - f2)^2
  ntest <- length(loss1)
  avgdiff <- mean(loss1 - loss2)
  
  varest <- drop(NeweyWest(lm(loss1 - loss2 ~ 1), lag = floor(ntest^.25),
                           adjust = TRUE, prewhite = FALSE))
  list(estimate = avgdiff,
       conf.int = c(avgdiff - sqrt(varest / ntest), Inf),
       p.value = pt(sqrt(ntest / varest) * avgdiff,
         ntest - 1, lower.tail = FALSE))
}

dframe <- as.data.frame(window(d, start = startyear, end = 2009))
RStart <- 20
REnd <- nrow(dframe) - 10
tstats <-    lapply(RStart:REnd, function(R) oosT(R, fullmodel, dframe, FALSE))
tstats.ct <- lapply(RStart:REnd, function(R) oosT(R, fullmodel, dframe, TRUE))

## make table assessing full-sample model
fullmod <- lm(fullmodel, data = dframe)
## assignment prevents display
temp <- latex(coeftest(fullmod, vcov=NeweyWest(fullmod, prewhite=FALSE, lag=2)),
              file = "tables/coeftest.tex", digits = 2)
temp <- latex(waldtest(fullmod, vcov=NeweyWest(fullmod, prewhite=FALSE, lag=2), test="F"),
              file = "tables/waldtest.tex", digits = 2)

oosStats <- function(tstats) 
  ts(cbind(average  = sapply(tstats, function(x) x$estimate),
           interval = sapply(tstats, function(x) min(x$conf.int)),
           pvalue   = sapply(tstats, function(x) x$p.value)),
     start = RStart, frequency = 1)

oos    <- oosStats(tstats)
oos.ct <- oosStats(tstats.ct)

## make plots of out-of-sample mse
## set parameters for pdf images
oosplot <- function(series, file, main, height = 3, width = 6.5,...) {
  pdf(file = file, height = height, width = width)
  x1 <- sum(is.na(series[,1])) + start(series)[1]
  xn <- end(series)[1]
  plot(series,
       plot.type = "single", bty = "n",
       xlab = "R", ylab = "MSE Difference",
       main = main,
       lwd = 0.8,
       col = c("white", "white"))
  polygon(x = c(x1, x1:xn, xn), y = c(0,series[,"interval"][!is.na(series[,"interval"])],0), col = "gray", lty = "blank")
  lines(start(series)[1]:end(series)[1], series[,"average"])
  ##  abline(h = 0, col = rgb(0,0,0,.3), lwd = 0.5)
  dev.off()
}

oosplot(oos[,c("interval", "average")],
        file = "plots/oos-mse-1.pdf",
        main = "Average OOS MSE Difference\nfor OLS Equity Premium Forecasts")

oos.subset <- oos
window(oos.subset, end = 49) <- NA
oosplot(oos.subset[,c("interval", "average")],
        file = "plots/oos-mse-1b.pdf",
        main = "Subset of Average OOS MSE Difference\nfor OLS Equity Premium Forecasts")        

oosplot(oos.ct[,c("interval", "average")],
        file = "plots/oos-mse-2.pdf",
        main = "Average OOS MSE Difference\nfor Restricted OLS Equity Premium Forecasts")

oos.subset <- oos.ct
window(oos.subset, end = 49) <- NA
oosplot(oos.subset[,c("interval", "average")],
        file = "plots/oos-mse-2b.pdf",
        main = "Subset of Average OOS MSE Difference\nfor Restricted OLS Equity Premium Forecasts")        


## plots of predictions.  Need to add the target variable to the
## graphs
forecastsframe <- function(R, mod.formula, dframe, nonneg = FALSE,...) {
  data.frame(estimated = startyear + R - 1,
             forecast  = forecasts(lm(mod.formula, data = dframe[1:R,]),
               dframe[-(1:R),], nonneg),
             date = R + seq(from = startyear,
               length = (nrow(dframe) - R)))
}

getforecasts <- function(RStart, REnd, nonneg = FALSE, ...) {
  dforecasts1 <- do.call(rbind, lapply(RStart:REnd, function(R)
                                       forecastsframe(R, equity.premium ~ 1,
                                                      dframe, nonneg)))
  dforecasts1$model <- "prevailing mean"
  dforecasts2 <- do.call(rbind, lapply(RStart:REnd, function(R)
                                       forecastsframe(R, fullmodel, dframe,
                                                      nonneg)))
  dforecasts2$model <- "kitchen sink"
  rbind(dforecasts1, dforecasts2)
}

forecastplot <- function(data, file, main, height = 6, width = 5,...) {
  pdf(file = file, height = height, width = width)
  trellis.par.set(list(axis.line = list(lwd = 0.25)))
  print(xyplot(forecast ~ date | model +
               equal.count(estimated, number = 5, overlap = 0),
               data = data,
               main = main,
               ylab = "Equity Premium",
               type = "l", groups = estimated, strip = FALSE,
               xlab = c("Kitchen Sink", "Prevailing Mean"),
               panel = function(x, y, ...) {
                 panel.xyplot(x, y, col = rgb(0,0,0,.5), lwd = .55, ...)
                 panel.lines(window(d[,"equity.premium"], end = 2009),
                             col = rgb(1,0,0,1), lwd = .55)
               },...))
       dev.off()
}

forecastplot(getforecasts(RStart, REnd, FALSE),
             file = "plots/forecastplot1.pdf",
             main = "OLS Forecasts of Equity Premium")

forecastplot(getforecasts(RStart, REnd, TRUE),
             file = "plots/forecastplot2.pdf",
             main = "Restricted Forecasts of Equity Premium")

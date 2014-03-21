library(sandwich)
library(lmtest)

dfull <- ts(read.csv("data/goyalwelch2009.csv")[,-1],
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
  list(avg1 = mean(loss1),
       avg2 = mean(loss2),
       estimate = avgdiff,
       conf.int = c(avgdiff - sqrt(varest / ntest), Inf),
       p.value = pt(sqrt(ntest / varest) * avgdiff,
         ntest - 1, lower.tail = FALSE))
}

dframe <- as.data.frame(window(d, start = startyear, end = 2009))
RStart <- 20
REnd <- nrow(dframe) - 10
tstats <-    lapply(RStart:REnd, function(R) oosT(R, fullmodel, dframe, FALSE))
tstats.ct <- lapply(RStart:REnd, function(R) oosT(R, fullmodel, dframe, TRUE))

oosStats <- function(tstats) 
  ts(cbind(PM = sapply(tstats, function(x) x$avg1),
           KS = sapply(tstats, function(x) x$avg2),           
           average  = sapply(tstats, function(x) x$estimate),
           interval = sapply(tstats, function(x) min(x$conf.int)),
           pvalue   = sapply(tstats, function(x) x$p.value)),
     start = RStart, frequency = 1)

oos    <- oosStats(tstats)
oos.ct <- oosStats(tstats.ct)

indplot <- function(name, series, file, height = 2, width = 5.5, xlabel = "",
                    ticks = NULL, labels = TRUE,...) {
  tikz(file = file, height = height, width = width)
  par(mar = c(5.1, 4.2, 1.1, 0))
  plot(cbind(series, 0), yaxt = "n", ylab = name,
       plot.type = "single", bty = "n",
       xlab = xlabel, lwd = 0.8,
       col = c("white", "white"))
  lines(start(series)[1]:end(series)[1], series)
  axis(2, at = ticks, labels = labels, cex.axis = .8, las = 1)
  dev.off()  
}

## make plots of out-of-sample mse
## set parameters for  images
oosplot <- function(series, file, height = 2, width = 5.5, xlabel = "",
                    ticks = NULL, labels = TRUE,...) {
  tikz(file = file, height = height, width = width)
  x1 <- sum(is.na(series[,1])) + start(series)[1]
  xn <- end(series)[1]
  par(mar = c(5.1, 4.2, 1.1, 0))
  plot(cbind(series, 0), yaxt = "n", 
       plot.type = "single", bty = "n",
       ylab = "MSE Diff.", xlab = xlabel,
       lwd = 0.8,
       col = c("white", "white"))
  polygon(x = c(x1, x1:xn, xn),
          y = c(0,series[,"interval"][!is.na(series[,"interval"])],0),
          col = "gray", lty = "blank")
  lines(c(start(series)[1], end(series)[1]), c(0,0), col = "gray")
  lines(start(series)[1]:end(series)[1], series[,"average"])
  axis(2, at = ticks, labels = labels, cex.axis = .8, las = 1)
  dev.off()
}

save.image("data/empirical-results.RData")

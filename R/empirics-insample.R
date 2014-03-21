library(dbframe)
library(MASS)
library(sandwich)
library(lmtest)
library(Hmisc)
load("data/empirical-results.RData")

## Unfortunately, the default formatting for these tables is pretty
## terrible. You need to edit them by hand! And it's set up to do
## that: we add commands here to generate all of the tables, so if you
## renew those macros before calling them, LaTeX will skip over these
## tables.

## Sort of a painful way to create this vector of variable names, but
## I like having the label and the text side by side.
regnames <- matrix(
  c("stock.variance", "Stock market variance",
    "book.to.market.ratio", "Book to market ratio",
    "dividend.price.ratio", "Dividend to price ratio (log)",
    "long.term.rate", "Long term rate",
    "long.term.yield", "Long term yield",
    "earnings.price.ratio", "Earnings to price ratio (log)",
    "inflation", "Inflation rate",
    "default.return.spread", "Default return spread",
    "net.equity.expansion", "Net equity expansion",
    "treasury.bill", "Treasury Bill rate",
    "percent.equity.issuing", "Percent equity issuing",
    "default.yield.spread", "Default yield spread"), ncol = 2, byrow = TRUE)
rownames(regnames) <- regnames[,1]
regnames <- regnames[,2]

regs <- attr(terms(fullmodel), "term.labels")
## studentize
dframe <-
  data.frame(equity.premium = 100 * dframe$equity.premium, ## basis points
             lapply(dframe[,regs],
                    function(x)
                    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))

fullmod <- lm(fullmodel, data = dframe)
vcv <- NeweyWest(fullmod, prewhite=FALSE, lag=2)

m1 <- lapply(regs, function(r) lm(dframe$equity.premium ~ dframe[,r]))
mf <- coeftest(fullmod, vcov = vcv)
regtable <- data.frame(variable = regnames[regs],
                       cf = mf[-1,1],
                       c1 = sapply(m1, function(u) coef(u)[2]),
                       pf = mf[-1,4],
                       p1 = sapply(m1, function(u) coef(summary(u))[2,4]))
regtable <- regtable[order(regtable$cf, decreasing = TRUE),]

cap1 <- "\\caption{Coefficient estimates for the ``kitchen sink'' model described
in Section xxx.}"
cap2 <- "\\caption{Overall model fit for the ``kitchen sink'' model described
in Section xxx.}"
labs <- "\\label{tab:gwinsample}"

cat(file = "floats/empirics-insample.tex",
    ## Indivual coefficient estimates:
    "\\newcommand{\\insamplecoefs}{%\n",
    "\\begin{table}[tb]\n",
    booktabs(regtable, align = c("l", rep("C", 4)),
             digits = c(0, 2, 2, 3, 3), numberformat = c(FALSE, rep(TRUE, 4))),
    cap1, "\n", labs,
    "\\end{table}}\n\n",
    "\\newcommand{\\insampletests}{%\n",
    "\\begin{table}[tb]\\centering\n",
    latexTabular(waldtest(fullmod, vcov = vcv, test = "F"), digits = 3), "\n",
    cap2, "\n", labs,
    "\\end{table}}\n\n")

## To get semi-multiplicity-robust critical value...
vcvd <-  diag(1/sqrt(diag(vcv[-1,-1])))
mcs <- mvrnorm(10000, rep(0, 12), vcvd %*% vcv[-1,-1] %*% vcvd)
quantile(apply(mcs, 1, function(a) max(abs(a))), .95)

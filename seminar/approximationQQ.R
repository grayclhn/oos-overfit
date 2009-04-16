library(lattice)
load("mcGlobal.Rdata")
load("approximation.Rdata")

## generate qq-plot for simulations vs. theoretical quantiles
png("approximationQQ.png")
qqmath(mc, col = "black", ylab = "Simulated Values", xlab = "Theoretical Values", aspect = "1")
dev.off()

png("approximationHist.png")
hist(mc, 70, freq = F, xlab = "Standardized Simulated Value", main = "")
curve(dnorm(x), n = 1000, from = -4, to = 4, col = "red", add = T, lwd = 2)
dev.off()

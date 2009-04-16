library(lattice)
load("mcGlobal.Rdata")
load("simulations.Rdata")

muOverfit <- 1/3
varOverfit <- (1/5 - muOverfit^2) / n

## generate qq-plot for simulations vs. theoretical quantiles
png("overfitQQ.png")
qqmath(mcOverfit, col = "black", ylab = "Simulated Values", xlab = "Theoretical Values (West 1996)",
       distribution = function(p) qnorm(p = p, mean = muOverfit, sd = sqrt(varOverfit)), aspect = "1")
dev.off()

png("overfitHist.png")
hist(mcOverfit[mcOverfit < 1], 70, freq = F, ylim = c(0, 20), xlab = "Simulated Value", main = "")
curve(dnorm(x, mean = muOverfit, sd = sqrt(varOverfit)), from = 0.25, to = 0.45, n = 1000, col = "red", add = T,
      lwd = 2)
dev.off()

library(lattice)
load("mcGlobal.Rdata")
load("simulations.Rdata")

muQuad <- 1/3
varQuad <- (1/5 - muQuad^2) / n

## generate qq-plot for simulations vs. theoretical quantiles
png("quadQQ.png")
qqmath(mcQuad, col = "black", ylab = "Simulated Values", xlab = "Theoretical Values (West 1996)",
       distribution = function(p) qnorm(p = p, mean = muQuad, sd = sqrt(varQuad)), aspect = "1")
dev.off()

png("quadHist.png")
hist(mcQuad, 70, freq = F, ylim = c(0, 20), xlab = "Simulated Value", main = "")
curve(dnorm(x, mean = muQuad, sd = sqrt(varQuad)), from = 0.25, to = 0.45, n = 1000, col = "red", add = T,
      lwd = 2)
dev.off()

load("mcGlobal.Rdata")

set.seed(30)
d <- rmc()

png("blankPlot.png")
plot(y ~ x, las = 1, data = d)
dev.off()

png("quadWindow.png")
quadModel <- lm(y ~ poly(x, 2), subset = estimationWindow, data = d)
plot(y~x, las = 1, type = "n", data = d)
points(y ~ x, subset = estimationWindow, data = d)
curve(predict(quadModel, data.frame(x = x)), add = T, from = 0, to = 3, n = 1000, col = "red", lwd = 2)
dev.off()

png("quadTest.png")
plot(y~x, las = 1, type = "n", data = d)
points(y ~ x, subset = -estimationWindow, data = d)
curve(predict(quadModel, data.frame(x = x)), add = T, from = 0, to = 3, n = 1000, col = "red", lwd = 2)
dev.off()

png("overfitPlot.png")
overfitModel <- lm(y ~ poly(x, K), subset = estimationWindow, data = d)
plot(y ~ x, las = 1, data = d)
curve(predict(overfitModel, data.frame(x = x)), add = T, from = 0, to = 3, n = 1000, col = "red", lwd = 2)
dev.off()

png("overfitAsymptotic.png")
plot(y ~ x, las = 1, data = rmc(4000))
curve(predict(overfitModel, data.frame(x = x)), add = T, from = 0, to = 3, n = 1000, col = "red", lwd = 2)
dev.off()

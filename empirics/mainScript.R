source("makeDataMatrices.R")

mainData <- read.csv("phillipsCurve.csv")
baseSeries <- ts(mainData[,c("unemployment", "inflation")], end = c(2008, 4), frequency = 12)

pdf("unemploymentPlot.pdf", width = 8.2, height = 3.7)
plot(baseSeries[,"unemployment"], main = "Unemployment Rate", ylab = "", xlab = "", las = 1)
dev.off()
pdf("inflationPlot.pdf", width = 8.2, height = 3.7)
plot(baseSeries[,"inflation"], main = "CPI Inflation (%)", ylab = "", xlab = "", las = 1)
dev.off()

dataMatrices <- makeDataMatrices(y = mainData$inflationChange, x = mainData$unemployment)

target <- dataMatrices$target
predictors <- dataMatrices$predictorMatrix
rm(list = c("mainData", "dataMatrices"))

n <- length(target)
rvec <- 108:(n-120)

rmse1 <- matrix(NA, length(rvec), 6)
rmse2 <- matrix(NA, length(rvec), 6)

testWindow <- seq(to = n, length = 120)

for (r in rvec) {
  estimationWindow2.r <- seq(to = testWindow[1] - 1, length = r)
  rmse1[r - rvec[1] + 1, 1] <- sqrt(mean((target[testWindow] - mean(target[1:r]))^2))
  rmse2[r - rvec[1] + 1, 1] <- sqrt(mean((target[testWindow] - mean(target[estimationWindow2.r]))^2))
  for (k in 1:5) {
    rmse1[r - rvec[1] + 1, k+1] <- sqrt(mean((target[testWindow] - predictors[testWindow, c(1, 1 + 1:k, 13 + 1:k)] %*%
                                              lm.fit(y = target[1:r],
                                                     x = predictors[1:r,
                                                       c(1, 1 + 1:k, 13 + 1:k)])$coef)^2))
    rmse2[r - rvec[1] + 1, k+1] <- sqrt(mean((target[testWindow] - predictors[testWindow, c(1, 1 + 1:k, 13 + 1:k)] %*%
                                              lm.fit(y = target[estimationWindow2.r],
                                                     x = predictors[estimationWindow2.r,
                                                       c(1, 1 + 1:k, 13 + 1:k)])$coef)^2))
  }
}

results <- data.frame(R = rvec, k = factor(paste("Lag =", rep(1:4, each = length(rvec)))), w1 = c(rmse1[,2:5]),
                      w2 = c(rmse2[,2:5]))

png(file = "blank.png", width = 900)
plot(c(rmse1[,5], rmse2[,5]) ~ rep(rvec,2), las = 1, ylab = "RMSE", xlab = "R", type = "n")
dev.off()

png(file = "rmses.png", width = 900)
xyplot(w1 + w2 ~ R | k, data = results, ylab = "RMSE", las = 1, col = c("black", "red"), type = "l",
       aspect = 1/2, as.table = TRUE)
dev.off()

for (k in 1:6) {
  pdf(file = paste("rmsePlots_", k-1, ".pdf", sep = ""), width = 8.5, height = 8)
  plot(rmse1[,k] ~ rvec, ylim = c(3.5, 4.15), type = "l", main = "", ylab = "", xlab = "", las = 1)
  lines(rmse2[,k] ~ rvec, col = "red")
  dev.off()
}
for (k in 1:6) {
  postscript(file = paste("rmsePlotsSmall_", k-1, ".ps", sep = ""), width = 1.75, height = 2.95)
  plot(rmse1[,k] ~ rvec, ylim = c(3.5, 4.15), type = "l", main = "", ylab = "", xlab = "", las = 1, omi = c(0,0,0,0),
       xaxt = "n", yaxt = "n", bty = "n")
  lines(rmse2[,k] ~ rvec, col = "red")
  dev.off()
}

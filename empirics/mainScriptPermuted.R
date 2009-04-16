source("makeDataMatrices.R")
slideHeight <- 8
slideWidth <- 13
slideMargin <- 0.75
slideGutter <- 0.2

gridHeight <- (slideHeight - 2 * slideMargin - 2 * slideGutter) / 3
gridWidth  <- (slideWidth  - 2 * slideMargin - 2 * slideGutter) / 3

mainData <- read.csv("phillipsCurve.csv")
seriesPermutation <- sample(dim(mainData)[1])
  
baseSeries <- ts(mainData[seriesPermutation,c("unemployment", "inflation")],
                 end = c(2008, 4), frequency = 12)

pdf("unemploymentPlot.pdf", width = 8.2, height = 3.7)
plot(baseSeries[,"unemployment"], main = "Unemployment Rate", ylab = "", xlab = "", las = 1)
dev.off()
pdf("inflationPlot.pdf", width = 8.2, height = 3.7)
plot(baseSeries[,"inflation"], main = "CPI Inflation (%)", ylab = "", xlab = "", las = 1)
dev.off()

dataMatrices <- makeDataMatrices(y = mainData$inflationChange[seriesPermutation],
                                 x = mainData$unemployment[seriesPermutation])

target <- dataMatrices$target
predictors <- dataMatrices$predictorMatrix
rm(list = c("mainData", "dataMatrices"))

n <- length(target)
rvec <- 108:(n-120)

rmse1 <- matrix(NA, length(rvec), 13)
rmse2 <- matrix(NA, length(rvec), 13)

testWindow <- seq(to = n, length = 120)

for (r in rvec) {
  estimationWindow2.r <- seq(to = testWindow[1] - 1, length = r)
  rmse1[r - rvec[1] + 1, 1] <- sqrt(mean((target[testWindow] - mean(target[1:r]))^2))
  rmse2[r - rvec[1] + 1, 1] <- sqrt(mean((target[testWindow] - mean(target[estimationWindow2.r]))^2))
  for (k in 1:12) {
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

for (k in 1:6) {
  pdf(file = paste("rmsePlots_", k-1, ".pdf", sep = ""), width = 7.6, height = 6.5)
  plot(rmse1[,k] ~ rvec, type = "l", ylim = c(min(c(rmse1[,k], rmse2[,k])), max(c(rmse1[,k], rmse2[,k]))),
       main = "", ylab = "", xlab = "", las = 1)
  lines(rmse2[,k] ~ rvec, col = "red")
  dev.off()
}

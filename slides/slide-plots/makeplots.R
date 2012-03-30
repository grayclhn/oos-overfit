set.seed(12)
f <- function(x) x
R <- 70
P <- 30
Q <- 1000
K <- 50
  
x <- runif(R + P + Q, max = 3)
y <- f(x) + rnorm(R+P+Q)
d <- data.frame(x, y)
rm(x,y)

dmock <- data.frame(x = seq(min(d$x), max(d$x), 0.01), y = NA)
dopar <- function(...)
  par(mfrow = c(1,2), las = 1, bty = "n", ...)

plotit <- function(subsample,
                   trainColor = rgb(1,1,1,0),
                   testColor = rgb(1,1,1,0),
                   linecolor = rgb(1,1,1,0)) {
  dmock <- data.frame(x = seq(min(d$x), max(d$x), 0.01), y = NA)
  
  trainingsample <- d[1:(min(subsample) - 1),]
  plotfn <- function(deg,...) {
    plot(y ~ x, data = d, type = "n",...)
    points(y ~ x, data = trainingsample, col = trainColor, lwd = 2)
    points(y ~ x, data = d, subset = subsample, col = testColor, lwd = 2)
    lines(lwd = 3, predict(lm(y ~ poly(x, deg, raw = T), trainingsample), dmock) ~ dmock$x, col = linecolor)
  }
  
  dopar()
  plotfn(1, main = "Model 1")
  plotfn(K, main = "Model 2")
}

dopdf <- function(file) pdf(file, height = 2.4/4 * 10, width = 10)

dopdf("overview-1.pdf")
plotit((R+P)+(1:Q), trainColor = "black")
dev.off()

dopdf("overview-2.pdf")
dopar()
plot(y ~ x, data = d, type = "n", main = "Model 1")
points(y ~ x, data = d[1:(R+P),], col = "black", lwd = 2)
lines(lwd = 3, predict(lm(y ~ x, d[1:(R+P),]), dmock) ~ dmock$x, col = "blue")
plot(y ~ x, data = d, type = "n", main = "Model 2")
points(y ~ x, data = d[1:(R+P),], col = "black", lwd = 2)
dev.off()

dopdf("overview-2b.pdf")
plotit((R+P) + (1:Q), linecolor = "blue", trainColor = "black")
dev.off()

## dopdf("overview-2c.pdf")
## dopar()
## plot(y ~ x, data = d, type = "n", main = "Model 1")
## points(y ~ x, data = d, col = "black", lwd = 2)
## lines(lwd = 3, predict(lm(y ~ x, d), dmock) ~ dmock$x, col = "blue")
## plot(y ~ x, data = d, type = "n", main = "Model 2")
## points(y ~ x, data = d, col = "black", lwd = 2)
## lines(lwd = 3, predict(lm(y ~ poly(x, K, raw = T), d), dmock) ~ dmock$x, col = "blue")
## dev.off()

## dopdf("overview-2d.pdf")
## dopar()
## plot(y ~ x, data = d, type = "n", main = "Model 1")
## points(y ~ x, data = d, col = "black", lwd = 2)
## lines(lwd = 3, predict(lm(y ~ x, d), dmock) ~ dmock$x, col = "blue")
## plot(y ~ x, data = d, type = "n", main = "Model 2")
## points(y ~ x, data = d, col = "black", lwd = 2)
## lines(lwd = 3, predict(lm(y ~ poly(x, 250, raw = T), d), dmock) ~ dmock$x, col = "blue")
## dev.off()

dopdf("overview-3.pdf")
plotit(R + (1:P), trainColor = "black", testColor = "black")
dev.off()

dopdf("overview-4.pdf")
plotit(R + (1:P), trainColor = "black", testColor = rgb(0,0,0,0.1),
       linecolor = rgb(0,0,1))
dev.off()

dopdf("overview-5.pdf")
plotit(R + (1:P), trainColor = rgb(0,0,0,0.1), testColor = "black", linecolor = rgb(0,0,1))
dev.off()

dopdf("asymptotics-1.pdf")
dopar()
plot(y ~ x, data = d, type = "n", main = "Model 1")
points(y ~ x, data = d[1:R,], col = rgb(0,0,0,0.1), lwd = 2)
points(y ~ x, data = d[R + (1:P),], col = "black", lwd = 2)
lines(lwd = 3, predict(lm(y ~ x, d[1:(R+P),]), dmock) ~ dmock$x, col = "blue")
lines(lwd = 3, c(0,3) ~ f(c(0,3)), col = "black")
plot(y ~ x, data = d, type = "n", main = "Model 2")
points(y ~ x, data = d[1:R,], col = rgb(0,0,0,0.1), lwd = 2)
points(y ~ x, data = d[R + (1:P),], col = "black", lwd = 2)
lines(lwd = 3, predict(lm(y ~ poly(x, K, raw = T), d[1:R,]), dmock) ~ dmock$x, col = "blue")
lines(lwd = 3, c(0,3) ~ f(c(0,3)), col = "black")
dev.off()

dopdf("asymptotics-1b.pdf")
plotit(R+(1:P), linecolor = rgb(0,0,1))
dev.off()

dopdf("trueOOS-1a.pdf")
plotit(R+P+(1:Q), trainColor = rgb(0,0,0,0.1), linecolor = "blue")
dev.off()
dopdf("trueOOS-1b.pdf")
plotit(R+P+(1:Q), trainColor = rgb(0,0,0,0.1), testColor = "red", linecolor = "blue")
dev.off()

dopdf("trueOOS-2.pdf")
plotit(R+P + (1:Q), testColor = rgb(1,0,0))
dev.off()


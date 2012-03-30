maxx <- 3
K <- 10

nsims <- 1500

n <- 200
estimationWindow <- 1:100


rmc <- function(n = 200) {
  x <- runif(n, max = maxx)
  e <- runif(n, min = -1, max = 1)
  y <- x^2 + e
  data.frame(y, x)
}

save.image("mcGlobal.Rdata")

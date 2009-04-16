makeDataMatrices <- function(y, x) {
  if (length(y) != length(x))
    stop("input vectors are not equal length.")
  
  n <- length(y) - 24

  target <- y[-(1:24)]
  predictorMatrix <- matrix(1, n, 25)
  for (i in 1:12) {
    predictorMatrix[,1+i] <- y[(1:n) + (13-i)]
    predictorMatrix[,13+i] <- x[(1:n) + (13-i)]
  }
  list(target = target, predictorMatrix = predictorMatrix)
}

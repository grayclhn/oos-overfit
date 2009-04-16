library(fixedWindow)
set.seed(92)

fstat <- function(y, xo, xa) {
### xo contains the regressors of the null model (including a constant)
### xa contains the regressors for the alternative (including a constant).
  d <- getDimensions(xo, xa)

  eo <- lm.fit(y = y, x = xo)$resid
  ea <- lm.fit(y = y, x = xa)$resid
  sum((eo^2 - ea^2)/d$m) / sum(ea^2/(d$n-d$ka))
}

getDimensions <- function(xo, xa) {
  if (!is.matrix(xo)) stop("the predictors of the null model must be a matrix")
  if (!is.matrix(xa)) stop("the predictors of the alternative model must be a matrix")
  if (! (dim(xa)[1] == dim(xo)[1])) stop("both matrices must have the same number of observations")
  if (dim(xa)[2] < dim(xo)[2]) stop("the null model must have fewer variables than the alternative.")
  r <- list(n = dim(xa)[1], ko = dim(xo)[2], ka = dim(xa)[2], m = dim(xa)[2] - dim(xo)[2],
            cn = (dim(xa)[2] - dim(xo)[2])/(dim(xa)[1] - dim(xa)[2]))
}

b <- 0.9
phi <- 0.5
gam <- 0.5

n <- 100
nburn <- 1000
### generate dm_t and dp_t

dm <- rep(0, n + nburn)
dp <- rep(0, n + nburn)
ds <- rep(0, n + nburn)

ep <- rnorm(n + nburn)
em <- rnorm(n + nburn)

for (t in 1:(n+nburn)) {
  dm[t+1] <- phi * dm[t] + em[t]
  dp[t+1] <- gam * dp[t] + ep[t]

  ds[t+1] <- phi*(1-b)/(1-b*phi) * dm[t] + 1/(1-b*phi) * em[t] +
    b*gam/(1-b*gam) * dp[t] + b/((1-b)*(1-b*gam)) * ep[t]
}

ds    <- ds[nburn + 1:n]
dmLag <- dm[nburn + 1:n - 1]
dpLag <- dp[nburn + 1:n - 1]

R <- 50
P <- n-R

calhounCI(pred0 = mean(ds[1:R]),
          predA = cbind(1, dmLag[1:P + R], dpLag[1:P + R]) %*%
          lm(ds ~ dmLag + dpLag, subset = 1:R)$coef,
          ds[1:P + R],
          coverage = 0.9)


1 - pf(fstat(ds, matrix(1, n, 1), cbind(rep(1,n), dmLag, dpLag)), 2, n-3)

cw.prederrors <- function(d, R) {
### uses the first R observations as an estimation window
### returns dataframe of recursive prediciton errors the two models
### used in Clark and West's (2007) DGP2.
  
  P <- dim(d)[1] - R
  e1 <- e2 <- f1 <- f2 <- rep(NA, P)
  c1.start <- lm.fit(y = d$y[2:R], x = cbind(1, d$y[2:R - 1]))$coef
  c2.start <- lm.fit(y = d$y[5:R], x = cbind(1, d$y[5:R - 1], d$y[5:R - 2], d$y[5:R - 3], d$y[5:R - 4],
                                     d$z[5:R - 1], d$z[5:R - 2], d$z[5:R - 3], d$z[5:R - 4]))$coef
  for (s in R + 0:(P-1)) {
    s1 <- 2:s
    s2 <- 5:s
    f1[s+1 - R] <- d$y[s+1] - crossprod(c(1, d$y[s]), c1.start)
    f2[s+1 - R] <- d$y[s+1] - crossprod(c(1, d$y[s - 0:3], d$z[s - 0:3]), c2.start) 
    e1[s+1 - R] <- d$y[s+1] - crossprod(c(1, d$y[s]),
                                        lm.fit(y = d$y[s1], x = cbind(1, d$y[s1 - 1]))$coef)
    e2[s+1 - R] <- d$y[s+1] - crossprod(c(1, d$y[s - 0:3], d$z[s - 0:3]),
                                        lm.fit(y = d$y[s2], x = cbind(1, d$y[s2-1], d$y[s2-2], d$y[s2-3],
                                                              d$y[s2-4], d$z[s2-1], d$z[s2-2], d$z[s2-3],
                                                              d$z[s2-4]))$coef)
  }
  ret <- data.frame(e1, e2, f1, f2)
  ret
}

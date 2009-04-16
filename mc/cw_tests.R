cw.tests <- function(Rvec, Pvec) {
  r <- rep(NA, length(Rvec) * length(Pvec))
  k <- 0
  
  d <- rcw(max(Rvec) + max(Pvec))
  for (R in Rvec) {
    s1 <- 2:R
    s2 <- 5:R
    c1 <- lm.fit(y = d$y[s1], x = cbind(1, d$y[s1 - 1]))$coef
    c2 <- lm.fit(y = d$y[s2], x = cbind(1, d$y[s2-1], d$y[s2-2], d$y[s2-3],
                                d$y[s2-4], d$z[s2-1], d$z[s2-2], d$z[s2-3],
                                d$z[s2-4]))$coef
    L1 <- rep(NA, max(Pvec))
    L2 <- rep(NA, max(Pvec))
    for (s in 1:max(Pvec)) {
      L1[s] <- (d$y[R+s] - crossprod(c(1, d$y[R+s-1]), c1))^2
      L2[s] <- (d$y[R+s] - crossprod(c(1, d$y[R+s - 1:4], d$z[R+s - 1:4]), c2))^2
    }
    for (P in Pvec) {
      t1 <- 2:(R+P)
      t2 <- 5:(R+P)
      avg.P <- lm((L1[1:P] - L2[1:P]) ~ 1)
      std.P <- sqrt(NeweyWest(avg.P)[1,1])
      ediff <- c(1, -1) %*% cw.eloss(lm.fit(y = d$y[t1], x = cbind(1, d$y[t1 - 1]))$coef,
                                     lm.fit(y = d$y[t2], x = cbind(1, d$y[t2-1], d$y[t2-2], d$y[t2-3],
                                                           d$y[t2-4], d$z[t2-1], d$z[t2-2], d$z[t2-3],
                                                           d$z[t2-4]))$coef)
      r[k <- k+1] <- !(ediff > avg.P$coef + 1.65 * std.P) & !(ediff < avg.P$coef - 1.65 * std.P)
    }
  }
  matrix(c(rep(Rvec,each = length(Pvec)), rep(Pvec, length(Rvec)), r), length(Rvec) * length(Pvec), 3)
}

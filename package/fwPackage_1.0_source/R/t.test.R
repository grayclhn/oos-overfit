t.test.oos.pair <- function(x,...)
  unname(mapply(function(e1, e2) t.test(e1^2 - e2^2, ...)$statistic,
                e1 = ForecastErrors(model.null(x)),
                e2 = ForecastErrors(model.alt(x))))

t.test.oos.forecast <- function(x,...)
  unname(mapply(function(e) t.test(e^2, ...)$statistic,
                e = ForecastErrors(x)))




oosPlot <- function(testquery,...) {
  nominal.size <- .1
  print(xyplot(size ~ ntest/ntotal | k + n, groups = b, auto.key = TRUE,
               data = ResultsFrame(paste("
SELECT AVG(teststat) as size, isim, idgp, ntest
FROM (", testquery, ")
GROUP BY isim, idgp, ntest", sep = '')),
               ylab = "Rejection Probability",
               xlab = "P/T",
               type = "l", ylim = c(-.1, 1.1), panel = function(x, y, ...) {
                 panel.xyplot(x, y, ...)
                 panel.lines(c(min(x), max(x)), c(nominal.size, nominal.size), col = rgb(0,0,0,0.3))
               },...))
}

## simple function: takes a dataframe x and returns a list where each
## element is a row of x

rows <- function(x)
  lapply(seq.int(nrow(x)), function(i) x[i,])

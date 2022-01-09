add2 <- function(x, y) {
  x+y
}

above <- function(x, n) {
  use <- x > n
  x[use]
}

columnname <- function(y, removeNA = T) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}
toBitArr <- function(x) {
  if (x != 0) {
    return(c(toBitArr(x %/% 2), c(x %% 2)))
  } else {
    return(x)
  }
}

fromBitArr <- function(x) {
  x <- as.integer(x)
  y <- rev(x)
  num <- as.integer64(0)
  i <- 0
  for (c in y) {
    num <- num + c * (2^i)
    i <- i + 1
  }
  return(num)
}


bOp <- function(x, y, op = NULL) {
  require(bit64)
  x <- as.integer64(x)
  y <- as.integer64(y)
  a <- toBitArr(x)
  b <- toBitArr(y)
  
  if (length(a) < length(b)) {
    t <- b
    b <- a
    a <- t
  }
  n <- length(a)
  m <- length(b)
  if (n != m) {
    b <- c(c(1:(n - m)) * as.integer64(0), b)
  }
  d <- c(1:n) * 0
  for (i in 1:n) {
    d[n - i + 1] <- xor(a[n - i + 1], b[n - i + 1])
  }
  return(fromBitArr(d))
}

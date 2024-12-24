input <- scan(read.input(22))
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
  num <- 0
  i <- 0
  for (c in y) {
    num <- num + c * (2^i)
    i <- i + 1
  }
  return(num)
}

process <- \(x){
  tb <- toBitArr(x)
  a <- as.logical(c(rep(0,6),tb))
  b <- as.logical(c(tb,rep(0,6)))
  d <- c()
  for(i in seq_along(a)) d[i] <- xor(a[i],b[i])
  if(length(d)<24) d <- c(rep(FALSE,24-length(d)),d)
  d <- d[length(d)-23:0]
  e <- c(rep(FALSE,5),d[1:(length(d)-5)])
  f <- c()
  for(i in seq_along(d)) f[i] <- xor(d[i],e[i])
  f <- f[length(f)-23:0]
  g <- c(f,rep(FALSE,11))
  f <- c(rep(FALSE,11),f)
  h <- c()
  for(i in seq_along(g)) h[i] <- xor(f[i],g[i])
  fromBitArr(h[length(h)-23:0])
}

res <- c()
for(i in seq_along(input)){
  x <- input[i]
  for(j in 1:2000){
    x <- process(x)
  }
  cat(i,"\r")
  res[i] <- x
}
sum(res)
#12664695565

seq_find <- \(x){
  dig <- c()
  for(j in 1:2000){
    x <- process(x)
    dig[j] <- x%%10
  }
  dig
}

res <- list()
for(i in seq_along(input)){
  a <-seq_find(input[i])
  res[[i]] <- cbind(a,diff(c(input[i]%%10,a)))
  cat(i,"\r")
}

tab <- list()
for(i in seq_along(res)){
  a <- c()
  for(j in 1:1997){
    a[j] <- paste(res[[i]][j+0:3,2],collapse="")
  }
  df <- data.frame(a=a,b=res[[i]][4:2000,1])
  df <- df[!duplicated(df$a),]
  tab[[i]] <- df
  cat(i,"\r")
}
dc <- do.call(rbind,tab)
max(sapply(split(dc$b,dc$a),sum))
#1444
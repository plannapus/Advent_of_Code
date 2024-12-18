input <- readLines(read.input(17))
#input <- c("Register A: 729","Register B: 0","Register C: 0","","Program: 0,1,5,4,3,0")
A <- as.integer(regmatches(input[1],gregexpr("[0-9]+",input[1]))[[1]])
B <- as.integer(regmatches(input[2],gregexpr("[0-9]+",input[2]))[[1]])
C <- as.integer(regmatches(input[3],gregexpr("[0-9]+",input[3]))[[1]])
prog <- as.integer(el(strsplit(gsub("Program: ","",input[5]),",")))
i <- 1
combo <- \(x){
  if(x%in%0:3) return(x)
  if(x==4) return(A)
  if(x==5) return(B)
  if(x==6) return(C)
}
output <- c()
while(i<=length(prog)){
  if(prog[i]==0){
    A <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }else if(prog[i]==1){
    B <- bitXor(B,prog[i+1])
    i <- i+2
  }else if(prog[i]==2){
    B <- combo(prog[i+1])%%8
    i <- i+2
  }else if(prog[i]==3){
    if(A==0){
      i <- i+2
    }else{
      i <- prog[i+1]+1
    }
  }else if(prog[i]==4){
    B <- bitXor(B,C)
    i <- i+2
  }else if(prog[i]==5){
    output <- c(output,combo(prog[i+1])%%8)
    i <- i+2
  }else if(prog[i]==6){
    B <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }else if(prog[i]==7){
    C <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }
  if(any(is.na(B))) stop()
}
cat(output,sep=",")
#7,1,2,3,2,6,7,2,5

options(digits=22)

# Following 3 functions from https://github.com/GjjvdBurg/R-bitops64
# as base R bitXor can't deal with integer >32 bits
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

opcode <- \(prog,A,B,C){
  combo <- \(x){
    if(x%in%0:3) return(as.integer64(x)) #I put as.integer64 everywhere because i m not sure which operator coerce what in what
    if(x==4) return(A)
    if(x==5) return(B)
    if(x==6) return(C)
  }
  i <- 1
  output <- c()
  while(i<=length(prog)){
    if(prog[i]==0){
      A <- as.integer64(floor(A/(2^combo(prog[i+1]))))
      i <- i+2
    }else if(prog[i]==1){
      B <- bOp(B,prog[i+1])
      i <- i+2
    }else if(prog[i]==2){
      B <- combo(prog[i+1])%%8
      i <- i+2
    }else if(prog[i]==3){
      if(A==as.integer64(0)){
        i <- i+2
      }else{
        i <- prog[i+1]+1
      }
    }else if(prog[i]==4){
      B <- bOp(B,C)
      i <- i+2
    }else if(prog[i]==5){
      output <- c(output,as.character(combo(prog[i+1])%%8))
      i <- i+2
    }else if(prog[i]==6){
      B <- as.integer64(floor(A/(2^combo(prog[i+1]))))
      i <- i+2
    }else if(prog[i]==7){
      C <- as.integer64(floor(A/(2^combo(prog[i+1]))))
      i <- i+2
    }
  }
  output
}

a <- b <- c <- 0
a <- as.integer64(a)
b <- as.integer64(b)
c <- as.integer64(c)
i <- 16

repeat{
  if(i==0) break
  for(j in 0:8){
    A <- a*as.integer64(8) + as.integer64(j)
    o <- opcode(prog,A,b,c)
    if(o[1]==prog[i]){
      a <- A
      i <- i-1
      break
    }
    cat(as.character(A),"\r")
  }
  cat("a=",as.character(a),"\n")
}
a
#202356708354602
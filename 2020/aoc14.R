options(digits=22)
input <- readLines("input14.txt")
mem <- c()
intTo36Bits <- function(x){
  res <- c()
  for(i in 35:0){
    res <- c(res,x%/%(2^i))
    x <- x%%(2^i)
  }
  res
}

bitsToInt<- function(x)sum(x*2^(35:0))

for(i in seq_along(input)){
  if(grepl("^mask",input[i])){
    mask <- el(strsplit(gsub("mask = ","",input[i]),""))
    w1 <- which(mask%in%"1")
    w0 <- which(mask%in%"0")
  }else{
    dig <- as.integer(el(regmatches(input[i],gregexpr("\\d+",input[i]))))
    index <- dig[1]
    value <- dig[2]
    b <- intTo36Bits(value)
    for(k in w1) b[k]<-1
    for(k in w0) b[k]<-0
    mem[index] <- bitsToInt(b)
    }
}
sum(mem,na.rm=TRUE)
#7611244640053

mem <- c()
for(i in seq_along(input)){
  if(grepl("^mask",input[i])){
    mask <- el(strsplit(gsub("mask = ","",input[i]),""))
    w1 <- which(mask%in%"1")
    wx <- which(mask%in%"X")
  }else{
    dig <- as.integer(el(regmatches(input[i],gregexpr("\\d+",input[i]))))
    index <- dig[1]
    value <- dig[2]
    b <- as.matrix(intTo36Bits(index))
    for(k in w1) b[k,]<-1
    for(k in wx){
      b <- cbind(b,b)
      b[k,] <- c(rep(0,ncol(b)/2),rep(1,ncol(b)/2))
    }
    #implied length of vectors too long, so instead a key-value matrix:
    mem <- rbind(mem,cbind(apply(b,2,bitsToInt),value))
    mem <- mem[!duplicated(mem[,1],fromLast=TRUE),]
  }
}
sum(mem[,2])
#3705162613854

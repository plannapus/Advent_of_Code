input <- 157623984
#input <- 389125467
input <- (input%%10^(9:1))%/%10^((9:1)-1)
turn <- function(x,n){
  start <- x[n]
  m <- (n+1:3)
  m[m>9] <- m[m>9]-9
  y <- x[m]
  x <-x[-m]
  r <- start-1
  if(r==0) r <- 9
  while(!r%in%x){
    r <- r-1
    if(r==0) r <- 9
  }
  w <- which(x==r)
  res <- `if`(w==length(x),c(x,y),c(x[1:w],y,x[(w+1):length(x)]))
  if(res[n]!=start){
    z <- which(res==start)
    res <- c(res[(z-n+1):length(res)],res[1:(z-n)])
  }
  res
}
n <- 1
for(i in 1:100){
  input <- turn(input,n)
  cat(i,":",input,"\n",sep="")
  n <- n+1
  if(n>9) n <- 1
}
z <- which(input==1)
if(z%in%c(1,9)){
  cat(input,sep="")
}else{
  cat(input[(z+1):length(input)],input[1:(z-1)],sep="")
}
#58427369

##Part2
input <- 157623984
#input <- 389125467
input <- (input%%10^(9:1))%/%10^((9:1)-1)
input <- c(input,10:1000000)
neighbours<-c(sapply(1:10,function(x)input[which(input==x)+1]),12:1000000,input[1])
start <- input[1]
for(i in 1:10000000){
  s1 <- neighbours[start]
  s2 <- neighbours[s1]
  s3 <- neighbours[s2]
  s4 <- neighbours[s3]
  neighbours[start]<-s4
  r <- ifelse(start-1,start-1,1000000)
  while(r%in%c(s1,s2,s3)) r <- ifelse(r-1,r-1,1000000)
  r1 <- neighbours[r]
  neighbours[r] <- s1
  neighbours[s3] <- r1
  start <- s4
  if(!i%%10000)cat(i,"\r")
}
x1 <- neighbours[1]
x2 <- neighbours[x1]
x1*x2
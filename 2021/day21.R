p <- c(5,9)
n <- 0
d <- 1:3
s <- c(0,0)
while(all(s<1000)){
  n <- n+3
  i <- ifelse(!n%%2,2,1)
  p[i] <- (p[i]+sum(d))%%10
  if(p[i]==0) p[i] <- 10
  s[i] <- s[i] + p[i]
  d <- d+3
  d[d>100] <- d[d>100]-100
}
n*s[s<1000]
#989352
library(bit64)
p <- c(5,9)
s <- c(0,0)
dirac <- apply(expand.grid(1:3,1:3,1:3),1,sum)
td <- integer64()
td[1:9] <- c(0,0,table(dirac))
turn <- function(p,s,d,i,dm){
  i <- as.integer(i)
  p[i] <- (p[i]+d)%%10
  if(p[i]==0) p[i] <- 10
  s[i] <- s[i] + p[i]
  w <- 0
  if(s[i]>=21) w <- i
  dm <- dm*td[d]
  i <- ifelse(i==1,2,1)
  res <- integer64(7)
  res[1:6]<-c(p,s,i,w)
  res[7] <- dm
  res
}
i=1
dm=as.integer64(1)
current <- do.call(rbind,lapply(3:9,function(x)turn(p,s,x,i,dm)))
W <- integer64(2)
repeat{
  nextt <- list()
  for(j in 1:nrow(current)){
    if(current[j,6]==0){
      nextt[[j]]<-do.call(rbind,
              lapply(3:9,function(x)turn(current[j,1:2],
                                           current[j,3:4],
                                           x,
                                           current[j,5],
                                           current[j,7])))
    }else{
      W[as.integer(current[j,6])] <- W[as.integer(current[j,6])]+current[j,7]
    }
  }
  current <- do.call(rbind,nextt)
  cat(nrow(current),":",as.character(W[1]),"-",as.character(W[2]),"\n")
  if(!nrow(current)) break
}
max(W)

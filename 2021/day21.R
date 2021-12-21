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
# #989352

#WIP
options(digits=22)
M <- array(0,dim=c(10,10,21,21))
M[5,9,1,1]<-1
W <- c(0,0)
dirac <- apply(expand.grid(1:3,1:3,1:3),1,sum)
td <- table(dirac)
while(sum(M)>0){
  N <- array(0,dim=c(10,10,21,21))
  for(i in 1:21){
    for(j in 1:21){
      for(k in 1:10){
        for(l in 1:10){
          if(M[k,l,i,j]>0){
            op <- M[k,l,i,j]
            for(m in 3:9){
              np1 <- k+m
              if(np1>10) np1 <- np1-10
              ns1 <- np1 + i-1
              if(ns1<21){
                for(n in 3:9){
                  np2 <- l+n
                  if(np2>10) np2 <- np2-10
                  ns2 <- np2 + j-1
                  if(ns2<21){
                    N[np1,np2,ns1+1,ns2+1]<-N[np1,np2,ns1+1,ns2+1]+op*td[m-2]*td[n-2]
                  }else{
                    W[2] <- W[2]+op*td[m-2]*td[n-2]
                  }
                }
              }else{
                W[1] <- W[1]+op*td[m-2]
              }
            }
          }
        }
      }
    }
  }
  M<-N
}

max(W)
#430229563871565
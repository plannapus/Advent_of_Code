input <- readLines("input.txt")
a <- parse.group("Button A: X(?<xa>[-+0-9]+), Y(?<ya>[-+0-9]+)",input[seq(1,length(input),4)])
a <- apply(a,2,as.integer)
b <- parse.group("Button B: X(?<xb>[-+0-9]+), Y(?<yb>[-+0-9]+)",input[seq(2,length(input),4)])
b <- apply(b,2,as.integer)
p <- parse.group("Prize: X=(?<xp>[-+0-9]+), Y=(?<yp>[-+0-9]+)",input[seq(3,length(input),4)])
p <- apply(p,2,as.integer)
map <- as.data.frame(cbind(a,b,p))

# map <- data.frame(xa=c(94,26,17,69),
#                   ya=c(34,66,86,23),
#                   xb=c(22,67,84,27),
#                   yb=c(67,21,37,71),
#                   xp=c(8400,12748,7870,18641),
#                   yp=c(5400,12176,6450,10279))

token <- 0
eg <- expand.grid(0:100,0:100)
for(i in 1:nrow(map)){
  w <- which((map$xp[i]-(eg[,1]*map$xa[i]+eg[,2]*map$xb[i])==0) & 
               (map$yp[i]-(eg[,1]*map$ya[i]+eg[,2]*map$yb[i])==0))
  if(length(w)){
    m <- min(eg[w,1]*3+eg[w,2])
    token <- token + m
  }
}
token
#33921

library(linprog)
options(digits=22)
map$xp <- 10000000000000+map$xp
map$yp <- 10000000000000+map$yp
token <- c()
for(i in 1:nrow(map)){
  a <- cbind(c(map$xa[i],map$ya[i]),c(map$xb[i],map$yb[i]))
  b <- cbind(c(map$xp[i],map$yp[i]))
  r1 <- solve(a,b)
  if(length(r1)& #Check that there is at least one solution here A and B are positive
     all(r1>0)&
     round(r1[1])*map$xa[i]+round(r1[2])*map$xb[i]==map$xp[i]&
     round(r1[1])*map$ya[i]+round(r1[2])*map$yb[i]==map$yp[i]){ #Now find the smallest possible
    r <- round(solveLP(c(3,1),c(b),a,const.dir=c("==","=="),lpSolve=TRUE,tol=1)$solution)
    token[i] <- sum(r*c(3,1))
  }
}
sum(token,na.rm=TRUE)
#82261957837868
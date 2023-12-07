input <- read.table(read.input(7),sep=" ",head=F)
spl <- strsplit(input[,1],"")
spl <- lapply(spl,factor,levels=c(2:9,"T","J","Q","K","A"))
s <- t(sapply(spl,table))
# o <- order(apply(s,1,max),
#       apply(s,1,\(x)sum(x==2)),
#       -xtfrm(apply(s,1,which.max)),
#       -xtfrm(apply(s,1,\(x)if(sum(x==2)==2){which(x==2)[2]}else{1})),
#       s[,1],s[,2],s[,3],s[,4],s[,5],s[,6],
#       s[,7],s[,8],s[,9],s[,10],s[,11],s[,12],decreasing=TRUE)
o <- order(apply(s,1,max),
           apply(s,1,\(x)sum(x==2)),
           sapply(spl,\(x)x[1]),
           sapply(spl,\(x)x[2]),
           sapply(spl,\(x)x[3]),
           sapply(spl,\(x)x[4]),
           sapply(spl,\(x)x[5]),
           decreasing=TRUE)
sum((1000:1)*input[o,2])
#250946742

spl <- strsplit(input[,1],"")
spl <- lapply(spl,factor,levels=c("J",2:9,"T","Q","K","A"))
s <- t(sapply(spl,table))
S<-s[,-1]
for(i in 1:nrow(s)){
  s2 <- rev(s[i,-1])
  j <- s[i,1]
  s2[which.max(s2)] <- s2[which.max(s2)]+j
  S[i,]<-rev(s2)
}
o <- order(apply(S,1,max),
           apply(S,1,\(x)sum(x==2)),
           sapply(spl,\(x)x[1]),
           sapply(spl,\(x)x[2]),
           sapply(spl,\(x)x[3]),
           sapply(spl,\(x)x[4]),
           sapply(spl,\(x)x[5]),
           decreasing=TRUE)
sum((1000:1)*input[o,2])
#251824095
input <-readLines("input24.txt")

b <- as.integer(gsub("add x (.+)$","\\1",grep("add x -?[0-9]+",input,v=TRUE)))
d26 <- as.integer(gsub("div z (.+)$","\\1",grep("div z [0-9]+",input,v=TRUE)))==26
a <- as.integer(gsub("add y (.+)$","\\1",grep("add y -?[0-9]+",input,v=TRUE)))
a<-a[!seq_along(a)%%3]

f<-function(n,i,z){
  if(!d26[i]){
    z <- (z*26)+n+a[i]
  }else{
    z2 <- z%/%26
    z <- z2*ifelse(n==((z%%26)+b[i]),1,26)+(n+a[i])*ifelse(n==((z%%26)+b[i]),0,1)
  }
  z
}
budget <- 26^sapply(1:14,function(x)sum(d26[x:14]))
dps <- function(i,z){
  if(i>14 & z==0) return("")
  if(i>14) return(c())
  if(z >= budget[i]) return(c())
  res <- c()
  for(w in 9:1){
    next_z <- f(w,i,z)
    res2 <- dps(i+1,next_z)
    for(x in res2){res <- c(res, paste0(w, x))}
    #if(i==1) cat(res,sep="\n")
  }
  res
}
r <- dps(1,0)
head(r,1) #99598963999971
tail(r,1) #93151411711211

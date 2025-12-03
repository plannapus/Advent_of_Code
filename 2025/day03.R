input <- readLines(read.input(3))
banks <- do.call(rbind,lapply(strsplit(input,""),as.integer))

bb <- c()
for(i in 1:200){
  bb[i]<-0
  n <- 9
  while(bb[i]==0){
    w <- which(banks[i,]==n)
    if(length(w)){
      if(w[1]==100){
        n <- n-1
      }else{
        bb[i] <- as.integer(paste0(n,max(banks[i,(w[1]+1):100])))
      }
    }else{n <- n-1}
  }
}
sum(bb)
#17694

nextbest <- \(bank,n,W){
  b <- 0
  m <- 9
  while(b==0){
    w <- W+which(bank[(W+1):100]==m)
    if(length(w)){
      if(w[1]>(101-n)){
        m <- m-1
      }else{
        b <- m
      }
    }else{m <- m-1}
  }
  list(pos=w[1],best=b)
}
options(digits=22)
bbb <- rep("",200)
for(i in 1:200){
  W <- 0
  for(j in 12:1){
    nb <- nextbest(banks[i,],j,W)
    bbb[i] <- as.numeric(paste0(bbb[i],nb$best))
    W <- nb$pos
  }
  #cat(bbb[i],"\n")
}
sum(bbb)
#175659236361660


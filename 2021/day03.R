input <- readLines("input03.txt")
tab <- do.call(rbind,strsplit(input,""))
TAB <- apply(tab,2,table)
gamma <- sum((apply(TAB,2,which.max)-1)*2^(11:0))
epsilon <- sum((apply(TAB,2,which.min)-1)*2^(11:0))
gamma*epsilon
#3320834
otab <- tab
i=1
repeat{
  OTAB <- apply(otab,2,function(x)table(factor(x,0:1)))
  if(OTAB[1,i]==OTAB[2,i]){w <- 1}else{w <- which.max(OTAB[,i])-1}
  otab <- otab[otab[,i]==w,,drop=FALSE]
  if(nrow(otab)==1) break
  i <- i+1
}
ox <- sum(as.integer(otab)*2^(11:0))
ctab <- tab
i=1
repeat{
  CTAB <- apply(ctab,2,function(x)table(factor(x,0:1)))
  if(CTAB[1,i]==CTAB[2,i]){w <- 0}else{w <- which.min(CTAB[,i])-1}
  ctab <- ctab[ctab[,i]==w,,drop=FALSE]
  if(nrow(ctab)==1) break
  i <- i+1
}
co <- sum(as.integer(ctab)*2^(11:0))
ox*co
#4481199
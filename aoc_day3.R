#Day 3 Puzzle 1
paths <- t(read.table("input3.txt",sep=",",header=FALSE))
path1<- path2<- c(0,0)
rd <- function(pt,x){
  dir <- substr(x,1,1)
  s <- switch(dir,"R"=c(0,1),"L"=c(0,-1),"U"=c(1,0),"D"=c(-1,0))
  n <- as.integer(gsub("[A-Z]","",x))
  if(n==0){
    return(NULL)
  }else{
    m<-matrix(s,nr=n+1,nc=2,byrow=T)
    m[1,]<-pt
    return(apply(m,2,cumsum)[-1,])
  }
}
for(i in 1:nrow(paths)){
  path1 <- rbind(path1,rd(tail(path1,1),paths[i,1]))
  path2 <- rbind(path2,rd(tail(path2,1),paths[i,2]))
}
p1 <- apply(path1,1,paste,collapse=" ")
p2 <- apply(path2,1,paste,collapse=" ")
int <- path1[which(p1%in%p2,),]
int <- int[-1,]
min(apply(int,1,function(x)sum(abs(x))))
#1017

#Day 3 Puzzle 2
w1<-which(p1%in%p2)
pint <- p1[p1%in%p2]  
w2<-sapply(pint,function(x)which(p2%in%x))
min(apply(cbind(w1,w2)[-1,],1,sum))-2
#11432

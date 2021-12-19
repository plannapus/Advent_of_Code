library(reshape)
input <- readLines("input19.txt")
w <- grep("^--",input)
scanners <- list()
for(i in seq_along(w)){
  if(i!=length(w)){
    scanners[[i]]<-read.table(text=input[(w[i]+1):(w[i+1]-2)],sep=",",header=FALSE)
  }else{
    scanners[[i]]<-read.table(text=input[(w[i]+1):length(input)],sep=",",header=FALSE)
  }
}
d <- lapply(scanners,dist)
common <- apply(combn(length(scanners),2),2,function(x)sum(d[[x[1]]]%in%d[[x[2]]]))
overlapping_pairs <- combn(length(scanners),2)[,common==66]
partial_overlap <- combn(length(scanners),2)[,common==15]

sloc <- matrix(NA,ncol=3,nrow=length(scanners))
sloc[1,]<-0
bloc <- matrix(NA,ncol=5,nrow=nrow(do.call(rbind,scanners)))
sl <- sapply(scanners,nrow)
bloc[,1]<-unlist(sapply(seq_along(sl),function(x)rep(x,sl[x])))
bloc[,2]<-unlist(lapply(sl,function(x)1:x))
bloc[bloc[,1]==1,3:5]<-as.matrix(scanners[[1]])
i <- 1
while(any(is.na(bloc))){
  op <- unique(c(overlapping_pairs[2,overlapping_pairs[1,]==i],
          overlapping_pairs[1,overlapping_pairs[2,]==i]))
  po <- unique(bloc[!is.na(bloc[,3]),1])
  op <- op[!op%in%po]
  for(j in op){
    m <- melt(as.matrix(dist(scanners[[i]])))
    n <- melt(as.matrix(dist(scanners[[j]])))
    m <- m[m[,3]!=0,]
    n <- n[n[,3]!=0,]
    o1 <- m[m[,3]%in%n[,3],]
    o2 <- n[n[,3]%in%m[,3],]
    u1 <- sort(unique(c(o1[,1],o1[,2])))
    u2 <- sort(unique(c(o2[,1],o2[,2])))
    mapping <- sapply(u1,function(x)table(factor(c(n[n[,3]%in%m[m[,1]==x|m[,2]==x,3],1],
                                 n[n[,3]%in%m[m[,1]==x|m[,2]==x,3],2]),u2)))
    u2 <- u2[apply(mapping,2,function(x)which(x==22))]
    a <- expand.grid(c(-1,1),c(-1,1),c(-1,1))
    perm <- rbind(c(1,2,3),c(1,3,2),c(2,1,3),c(2,3,1),c(3,1,2),c(3,2,1))
    A <- apply(perm,1,
               function(y)apply(a,1,function(x){
                 M <- t(t(scanners[[j]][u2,y])*x)-bloc[bloc[,1]==i,][u1,3:5]
                 M <- M[!duplicated(M),,drop=FALSE]
                 nrow(M)==1
               }))
    w <- which(A,arr.ind=T)
    aa <- c(a[w[1],1],a[w[1],2],a[w[1],3])
    p <- perm[w[2],]
    sloc[j,] <- unlist((t(t(scanners[[j]][u2,p])*aa)-bloc[bloc[,1]==i,][u1,3:5])[1,])
    t(t(scanners[[j]][,p])*aa-sloc[j,])->bloc[bloc[,1]==j,3:5]
  }
  rest <- unique(bloc[is.na(bloc[,3]),1])
  po <- unique(bloc[!is.na(bloc[,3]),1])
  g<-sapply(po,function(x)sum(unique(c(overlapping_pairs[2,overlapping_pairs[1,]==x],
                                overlapping_pairs[1,overlapping_pairs[2,]==x]))%in%rest))
  i = head(po[which.max(g)],1)
  cat(i,sep="\n")
}
sum(!duplicated(bloc[,3:5]))
#367
max(dist(sloc,"manhattan"))
#11925
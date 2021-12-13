input<-readLines("input13.txt")
w<-which(input=="")
pts <- read.table(text=input[1:(w-1)],sep=",")
fold <- read.table(text=input[(w+1):length(input)],sep="=")
range(pts)
M<-matrix(FALSE,nrow=max(pts[,1])+1,ncol=max(pts[,2])+1)
for(i in 1:nrow(pts)){
  M[pts[i,1]+1,pts[i,2]+1]<-TRUE
}
fold[1,]
#fold along x 655
Mf <- M[1:655,]
Mf<-Mf|M[nrow(M):657,]
sum(Mf)
#818
M <- Mf
for(i in 2:nrow(fold)){
  if(grepl("x$",fold[i,1])){
    Mf <- M[1:fold[i,2],]
    Mf<-Mf|M[nrow(M):(fold[i,2]+2),]
    M <- Mf
  }else{
    Mf <- M[,1:fold[i,2]]
    Mf<-Mf|M[,ncol(M):(fold[i,2]+2)]
    M <- Mf
  }
}
par(mar=c(0,0,0,0))
image(M[1:40,6:1])
#LRGPRECB
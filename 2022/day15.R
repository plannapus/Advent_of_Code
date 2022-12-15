input <- readLines("input15.txt")
parsed <- parse.group("Sensor at x=(?<sx>-?[0-9]+), y=(?<sy>-?[0-9]+): closest beacon is at x=(?<bx>-?[0-9]+), y=(?<by>-?[0-9]+)",input)
coord <- as.data.frame(apply(parsed,2,as.integer))
coord$manh <- abs(coord$sx-coord$bx)+abs(coord$sy-coord$by)
rowy<-2000000
u<-c()
for(i in 1:nrow(coord)){
  d<-coord$manh[i]-abs(rowy-coord$sy[i])
  if(d>=0){
    n<-(coord$sx[i]-d):(coord$sx[i]+d)
    u<-unique(c(u,n))
  }
}
length(u[!u%in%coord$bx[coord$by==rowy]])
#5832528

##Second part, does not work yet:
M <- 4000000
yr <- list()
for(i in 0:M) yr[[i+1]]<-matrix(ncol=2,nrow=0)

for(i in 1:nrow(coord)){
 for(k in 0:coord$manh[i]){
   xm <- pmax(0,coord$sx[i]-k)
   xM <- pmin(M,coord$sx[i]+k)
   if(coord$sy[i]-k>=0){
     yr[[coord$sy[i]-k+1]]<-rbind(yr[[coord$sy[i]-k+1]],c(xm,xM))
   }
   if(coord$sy[i]+k<=M){
     yr[[coord$sy[i]+k+1]]<-rbind(yr[[coord$sy[i]+k+1]],c(xm,xM))
   }
   if(!k%%10000)cat(i,":",k,"\r")
 }
}

v <- 0:M
for(k in v){
  Y <- yr[[k+1]]
  M <- apply(Y,1,function(x)v<x[1]|v>x[2])
  N <- colSums(M)
  if(any(N==0)){
    x <- which(N==0)+1
    stop(x*M+k)
  }
  if(!k%%100)cat(k,"\r")
}

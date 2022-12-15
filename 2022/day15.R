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

##Second part:
M <- 4000000
yr <- list()
for(i in 0:M) yr[[i+1]]<-matrix(ncol=2,nrow=0)

for(i in 1:nrow(coord)){ #For each y, make matrix of x ranges reached by each sensor
 for(k in 0:coord$manh[i]){
   xm <- pmax(0,coord$sx[i]-k)
   xM <- pmin(M,coord$sx[i]+k)
   dk <- coord$manh[i]-k
   if(coord$sy[i]-dk>=0){
     yr[[coord$sy[i]-dk+1]]<-rbind(yr[[coord$sy[i]-dk+1]],c(xm,xM))
   }
   if(coord$sy[i]+dk<=M){
     yr[[coord$sy[i]+dk+1]]<-rbind(yr[[coord$sy[i]+dk+1]],c(xm,xM))
   }
   if(!k%%10000)cat(i,":",k,"\r")
 }
}

for(k in 0:M){
  Y <- yr[[k+1]]
  Y <- Y[order(Y[,1]),] #Order the matrix of ranges covered by each sensors at y=k
  j <- 1
  while(j<nrow(Y)){ #Getting rid of ranges fully included in previous one
    w <- c(rep(FALSE,j),Y[(j+1):nrow(Y),2]<Y[j,2])
    Y <- Y[!w,]
    j <- j+1
  }
  j <-1
  while(j<nrow(Y)){ # Merging overlapping ranges
    if(Y[j+1,1]<=Y[j,2]){
      Y[j,2] <- Y[j+1,2]
      Y <- Y[-(j+1),,drop=FALSE]
    }else{ #If two subsequent ranges don t overlap, then we found the sot
      stop((Y[j,2]+1)*M+k)
    }
  }
  if(!k%%1000)cat(k,"\r") #Step counter
}
#13360899249595
#y=3249595
#x=3340224
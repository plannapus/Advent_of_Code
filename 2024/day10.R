map <- read.map(10)
#map <- do.call(rbind,strsplit(readLines("test10.txt"),""))
map <- apply(map,2,as.integer)
h <- which(map==0,arr.ind=TRUE)
neigh <- \(x){
  n <- rbind(x+c(0,1),x+c(1,0),x+c(-1,0),x+c(0,-1))
  n[n[,1]>0&n[,2]>0&n[,2]<=ncol(map)&n[,1]<=nrow(map),,drop=FALSE]
}
recurs <- \(pos,nines){
  v <- map[pos[1],pos[2]]
  if(v==9) return(pos)
  n <- neigh(pos)
  vn <- apply(n,1,\(x)map[x[1],x[2]])
  if(!any(vn%in%(v+1))) return(c())
  n <- n[vn==(v+1),,drop=FALSE]
  for(i in 1:nrow(n)){
    nines <- rbind(nines,recurs(n[i,],c()))
  }
  return(nines)
}

trails <- c()
for(i in 1:nrow(h)){
  trails[i] <- nrow(unique(recurs(h[i,],c())))
}
sum(trails)
#822


recurs <- \(pos,t){
  v <- map[pos[1],pos[2]]
  if(v==9) return(1)
  n <- neigh(pos)
  vn <- apply(n,1,\(x)map[x[1],x[2]])
  if(!any(vn%in%(v+1))) return(0)
  n <- n[vn==(v+1),,drop=FALSE]
  for(i in 1:nrow(n)){
    t <- t+recurs(n[i,],0)
  }
  return(t)
}

trails <- c()
for(i in 1:nrow(h)){
  trails[i] <- recurs(h[i,],0)
}
sum(trails)
#1801
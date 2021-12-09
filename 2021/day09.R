input <- do.call(rbind,strsplit(readLines("input09.txt"),""))
#input <- do.call(rbind,strsplit(readLines("test09.txt"),""))
map <- apply(input,2,as.integer)
lowpoints <- c()
coords <- matrix(ncol=2)
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    c1 <- ifelse(i!=1,map[i,j]<map[i-1,j],TRUE)
    c2 <- ifelse(i!=nrow(map),map[i,j]<map[i+1,j],TRUE)
    c3 <- ifelse(j!=1,map[i,j]<map[i,j-1],TRUE)
    c4 <- ifelse(j!=ncol(map),map[i,j]<map[i,j+1],TRUE)
    if(c1&c2&c3&c4){
      lowpoints <- c(lowpoints, map[i,j])
      coords<-rbind(coords,c(i,j))
    }
  }
}
sum(lowpoints+1)
# 512

coords <- coords[-1,]
mapBasin <- array(NA,dim=dim(map))
whichBasin <- function(i,j){
  if(map[i,j]==9) return(NA)
  coord_to_check <- rbind(c(i-1,j),c(i+1,j),c(i,j-1),c(i,j+1))
  coord_to_check <- coord_to_check[!coord_to_check[,1]%in%c(0,nrow(map)+1)&
                                     !coord_to_check[,2]%in%c(0,ncol(map)+1),]
  w <- apply(coord_to_check,1,function(x)map[x[1],x[2]]<map[i,j])
  v <- apply(coord_to_check,1,function(x)map[x[1],x[2]])
  if(all(!w)){
    return(which(coords[,1]==i&coords[,2]==j))
  }else{
    b <- whichBasin(coord_to_check[which.min(v),1],coord_to_check[which.min(v),2])
    return(b)
  }
}
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    mapBasin[i,j] <- whichBasin(i,j)
  }
}
by_basin <- sapply(split(map,mapBasin),length)
prod(tail(sort(by_basin),3))
#1600104

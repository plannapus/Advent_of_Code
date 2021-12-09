input <- do.call(rbind,strsplit(readLines("input09.txt"),""))
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
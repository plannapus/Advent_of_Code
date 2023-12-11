map <- do.call(rbind,strsplit(readLines(read.input(11)),""))
input <- map
i <- 1
while(i<ncol(map)){
  if(all(map[,i]==".")){
    map <- cbind(map[,1:i],".",map[,(i+1):ncol(map)])
    i <- i+2
  }else{
    i <- i+1 
  }
}
i <- 1
while(i<nrow(map)){
  if(all(map[i,]==".")){
    map <- rbind(map[1:i,],".",map[(i+1):nrow(map),])
    i <- i+2
  }else{
    i <- i+1 
  }
}
galaxies <- which(map=="#",arr.ind=TRUE)
sum(dist(galaxies,"manhattan"))
# 9724940

options(digits=22)
map <- input
s <- 0
C <- which(colSums(map==".")==nrow(map))
R <- which(rowSums(map==".")==ncol(map))
galaxies <- which(map=="#",arr.ind=TRUE)
for(i in 1:(nrow(galaxies)-1)){
  for(j in (i+1):nrow(galaxies)){
    d <- abs(galaxies[i,1]-galaxies[j,1])+
      999999*sum((galaxies[i,1]:galaxies[j,1])%in%R)+
      abs(galaxies[i,2]-galaxies[j,2])+
      999999*sum((galaxies[i,2]:galaxies[j,2])%in%C)
    s <- s+d
  }
}
s
#569052586852  

#Day 6 Puzzle 1
map <- readLines("input6.txt")
map <- do.call(rbind,strsplit(map,")"))
obj <- unique(matrix(map,nc=1))
n<-0
for(i in seq_along(obj)){
  if(obj[i]%in%map[,2]){
    around <- map[map[,2]==obj[i],1]
    n <- n + 1
  }
  while(around%in%map[,2]){
    around <- map[map[,2]==around,1]
    n <- n + 1
  }
}
n
#300598

#Day 6 Puzzle 2
library(igraph)
map <- scan("input6.txt",what="character",sep=")")
gr <- make_undirected_graph(map)
d<-distances(gr,v=V(gr),to=V(gr))
d["YOU","SAN"]-2
#520

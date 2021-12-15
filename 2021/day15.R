map <- apply(do.call(rbind,strsplit(readLines("input15.txt"),"")),2,as.integer)
all_coords <- expand.grid(1:100,1:100)
neighbours <- function(x){
  coords <- rbind(c(x[1]-1,x[2]),
                  c(x[1]+1,x[2]),
                  c(x[1],x[2]-1),
                  c(x[1],x[2]+1))
  coords <- coords[!coords[,1]%in%c(0,101)&!coords[,2]%in%c(0,101),]
  ids <- apply(coords,1,function(z)which(all_coords[,1]==z[1]&all_coords[,2]==z[2]))
  id <- which(all_coords[,1]==x[1]&all_coords[,2]==x[2])
  res <- cbind(id,ids,apply(coords,1,function(z)map[z[1],z[2]]))
  res <- as.data.frame(res)
  colnames(res)<-c("from","to","value")
  res
}
nb <- do.call(rbind,apply(all_coords,1,neighbours))
nodes <- seq_len(nrow(coords))
g <- graph_from_edgelist(as.matrix(nb[,1:2]))
distances(g,1,10000,weights=g$value,algorithm="dijkstra")

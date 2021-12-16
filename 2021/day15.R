library(igraph)
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
g <- graph_from_edgelist(as.matrix(nb[,1:2]))
E(g)$weight <- nb$value
distances(g,1,nrow(map)*ncol(map),mode="out")
#540

MAP <- matrix(NA,nrow=500,ncol=500)
# I know, I know, it's insanely ugly :)
MAP[1:100,1:100]<-map
m <- map+1
m[m==10]<-1
MAP[101:200,1:100]<-m
MAP[1:100,101:200]<-m
m <- m+1
m[m==10]<-1
MAP[201:300,1:100]<-m
MAP[101:200,101:200]<-m
MAP[1:100,201:300]<-m
m <- m+1
m[m==10]<-1
MAP[301:400,1:100]<-m
MAP[201:300,101:200]<-m
MAP[101:200,201:300]<-m
MAP[1:100,301:400]<-m
m <- m+1
m[m==10]<-1
MAP[401:500,1:100]<-m
MAP[301:400,101:200]<-m
MAP[201:300,201:300]<-m
MAP[101:200,301:400]<-m
MAP[1:100,401:500]<-m
m <- m+1
m[m==10]<-1
MAP[401:500,101:200]<-m
MAP[301:400,201:300]<-m
MAP[201:300,301:400]<-m
MAP[101:200,401:500]<-m
m <- m+1
m[m==10]<-1
MAP[401:500,201:300]<-m
MAP[301:400,301:400]<-m
MAP[201:300,401:500]<-m
m <- m+1
m[m==10]<-1
MAP[401:500,301:400]<-m
MAP[301:400,401:500]<-m
m <- m+1
m[m==10]<-1
MAP[401:500,401:500]<-m

g <- as.directed(make_lattice(c(nrow(MAP), ncol(MAP))),mode = 'mutual')
E(g)$weight <- MAP[get.edgelist(g)[, 2]]
distances(g,"1",nrow(MAP)*ncol(MAP),mode="out")
#2879

#Visualization
par(mar=c(0,0,0,0))
image(seq_len(500),seq_len(500),MAP,col=rev(hcl.colors(9,"ag_GrnYl")),ylim=c(nrow(MAP),1))
path <- matrix(NA,nrow=500,ncol=500)
path[as.integer(el(shortest_paths(g,"1",nrow(MAP)*ncol(MAP),mode="out")$vpath))]<-1
image(seq_len(500),seq_len(500),path,breaks=c(0,1),col=c("black"),add=TRUE)

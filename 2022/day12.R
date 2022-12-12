map <- do.call(rbind,strsplit(readLines("input12.txt"),""))
start <- which(map=="S",arr.ind=T)
end <- which(map=="E",arr.ind=T)
map[map=="S"]<-"a"
map[map=="E"]<-"z"
all_coords <- expand.grid(1:nrow(map),1:ncol(map))
nb <- function(x,y){
    coords <- rbind(c(x-1,y),
                    c(x+1,y),
                    c(x,y-1),
                    c(x,y+1))
    coords <- coords[!coords[,1]%in%c(0,nrow(map)+1)&!coords[,2]%in%c(0,ncol(map)+1),]
    w <- which(letters==map[x,y])
    l <- apply(coords,1,function(z)which(letters==map[z[1],z[2]]))
    coords <- coords[l-w<=1,,drop=FALSE]
    if(nrow(coords)){
      ids <- apply(coords,1,function(z)which(all_coords[,1]==z[1]&all_coords[,2]==z[2]))
      id <- which(all_coords[,1]==x&all_coords[,2]==y)
      return(data.frame("from"=id,"to"=ids))
    }else{return(NULL)}
}
library(igraph)
edg <- do.call(rbind,apply(all_coords,1,function(x)nb(x[1],x[2])))
g <- graph_from_edgelist(as.matrix(edg)) #Make a directed graph with list of possible paths
distances(g,
          which(all_coords[,1]==start[1]&all_coords[,2]==start[2]),
          which(all_coords[,1]==end[1]&all_coords[,2]==end[2]),mode="out")
#394
sp <- apply(which(map=="a",arr.ind=T),1,function(x){
  which(all_coords[,1]==x[1]&all_coords[,2]==x[2])
})
d<-distances(g,
          sp,
          which(all_coords[,1]==end[1]&all_coords[,2]==end[2]),mode="out")
min(d)
#388


#Visualization
png("plot_day12a.png",h=1045,w=550)
par(mar=c(0,0,0,0))
image(1:nrow(map),1:ncol(map),map2,asp=1,breaks=0:26,col=hcl.colors(26,"ag_GrnYl"))
text(start[1],start[2],"S")
text(end[1],end[2],"E")
s <- shortest_paths(g,
               which(all_coords[,1]==start[1]&all_coords[,2]==start[2]),
               which(all_coords[,1]==end[1]&all_coords[,2]==end[2]),mode="out")
points(all_coords[el(s$vp),],cex=0.5,pch=15)
dev.off()

png("plot_day12b.png",h=1045,w=550)
par(mar=c(0,0,0,0))
image(1:nrow(map),1:ncol(map),map2,asp=1,breaks=0:26,col=hcl.colors(26,"ag_GrnYl"))
text(start[1],start[2],"S")
text(end[1],end[2],"E")
s <- shortest_paths(g,
               sp[which.min(d)],
               which(all_coords[,1]==end[1]&all_coords[,2]==end[2]),mode="out")
points(all_coords[el(s$vp),],cex=0.5,pch=15,col="red")
dev.off()

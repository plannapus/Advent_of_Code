library(igraph)
input <- do.call(rbind,strsplit(readLines(read.input(10)),""))
edges <- matrix(ncol=2)
for(i in 1:nrow(input)){
  for(j in 1:ncol(input)){
    s <- input[i,j]
    v <- sprintf("%03d%03d",i,j)
    if(s=="S") start <- v
    if(s%in%c("|","L","J","S")){
      edges <- rbind(edges,c(v,sprintf("%03d%03d",i-1,j)))
    }
    if(s%in%c("|","7","F","S")){
      edges <- rbind(edges,c(v,sprintf("%03d%03d",i+1,j)))
    }
    if(s%in%c("J","7","-")){
      edges <- rbind(edges,c(v,sprintf("%03d%03d",i,j-1)))
    }
    if(s%in%c("L","F","-")){
      edges <- rbind(edges,c(v,sprintf("%03d%03d",i,j+1)))
    }
  }
}
g <- graph_from_edgelist(edges[-1,],directed=TRUE)
d <- distances(g, v=start,mode="in")
max(d[is.finite(d)],na.rm=TRUE)
#6909
# 
asd <- all_simple_paths(g, start,"103061" , mode="out")
l <- lapply(asd,names)
ring <- c(l[[1]][-1],rev(l[[2]])[-1])
poly <- do.call(rbind,lapply(ring,\(x)c(as.integer(substr(x,1,3)),as.integer(substr(x,4,6)))))
# pts <- expand.grid(1:140,1:140)
# library(splancs)
# inside <- pip(pts,poly,out=FALSE,bound=FALSE)
# nrow(inside)
map <- input
map[] <- "."
for(i in 1:nrow(poly)){
  map[poly[i,1],poly[i,2]]<-input[poly[i,1],poly[i,2]]
}
n <- 0
pts <- c()
for(i in 1:nrow(map)){
  inside <- FALSE
  for(j in 1:ncol(map)){
    if(map[i,j] %in% c("|","L","J","S")) inside <- !inside
    if(map[i,j]=="." & inside){
      n <- n + 1
      pts <- rbind(pts, c(i,j))
    }
  }
}
n
#461


# 
###Visualizations
png("visualizations/day10.png",h=1200,w=1200)
par(mar=c(0,0,0,0))
plot(NA,xlim=c(0,141),ylim=c(0,141),ann=F,ax=F)
polygon(poly,col="grey")
points(pts,pch=19,col="darkgreen")
dev.off()

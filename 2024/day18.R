#input <- read.table(read.input(18),sep=",")
input <- read.table("input.txt",sep=",")
map <- matrix(TRUE,nrow=71,ncol=71)
mtt <- list()

for(i in 1:nrow(input)){
  if(i==1){
    mtt[[i]] <- map
  }else{
    mtt[[i]] <- mtt[[i-1]]
  }
  mtt[[i]][input[i,1]+1,input[i,2]+1] <- FALSE
}

neigh <- \(i,j,m){
  n <- matrix(ncol=2,nrow=0)
  if(i>1) n <- rbind(n, c(i-1,j))
  if(i<71) n <- rbind(n, c(i+1,j))
  if(j>1) n <- rbind(n, c(i,j-1))
  if(j<71) n <- rbind(n, c(i,j+1))
  n[apply(n,1,\(x)m[x[1],x[2]]),,drop=FALSE]
}

m <- mtt[[1024]]
edges <- matrix(ncol=2,nrow=0)
for(i in 1:nrow(m)){
  for(j in 1:ncol(m)){
    n <- neigh(i,j,m)
    if(nrow(n)>0){
      for(k in 1:nrow(n)){
        edges <- rbind(edges,c(i*100+j,n[k,1]*100+n[k,2]))
      } 
    }
  }
}
library(igraph)
g <- graph_from_edgelist(edges)
length(el(shortest_paths(g,from="101",to="7171")$vpath))-1
#248

for(b in 1025:3450){
  nb <- (input[b,1]+1)*100+(input[b,2]+1)
  edges <- edges[!(edges[,1]==nb|edges[,2]==nb),]
  g <- graph_from_edgelist(edges)
  s<-shortest_paths(g,from="101",to="7171")
  if(length(s$vpath[[1]])==0)break
  cat(b,"\r")
}
paste(input[b,],collapse=",")
#32,55



#### Visualizations:
dir.create("png")
setwd("png")
for(b in 1:3450){
  png(sprintf("%04i.png",b),w=400,h=400)
  par(mar=c(0,0,0,0))
  image(1:71,1:71,t(mtt[[b]]),ylim=c(71,1))
  dev.off()
}
system("convert -delay 5 -loop 1 *.png ../day18.gif")
setwd("..")
unlink("png",recursive=TRUE)
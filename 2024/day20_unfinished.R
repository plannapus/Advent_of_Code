library(igraph)
source("read.input.R")
map <- read.map(20)
start <- which(map=="S",arr.ind=TRUE)
path <- which(map==".",arr.ind=TRUE)
end <- which(map=="E",arr.ind=TRUE)
path <- rbind(start,path,end)
walls <- which(map=="#",arr.ind=TRUE)

neigh <- \(x){
  n <- rbind(x+c(0,1),x+c(0,-1),x+c(1,0),x+c(-1,0))
  n <- n[n[,1]>0&n[,2]>0&n[,1]<142&n[,2]<142,,drop=FALSE]
  j <- c()
  for(i in 1:nrow(n)){
    if(!any(path[,1]==n[i,1]&path[,2]==n[i,2])) j <- c(j,i)
  }
  n[-j,,drop=FALSE]
}

edges <- matrix(ncol=2,nrow=0)
for(i in 1:nrow(path)){
  n <- neigh(path[i,])
  p <- paste(path[i,1],path[i,2],sep="_")
  np <- apply(n,1,\(x)paste(x[1],x[2],sep="_"))
  edges <- rbind(edges, cbind(p,np))
}
g <- graph_from_edgelist(edges)
s <- paste(start[1],start[2],sep="_")
e <- paste(end[1],end[2],sep="_")
t0 <- length(shortest_paths(g,s,e)$vpath[[1]])
normal_path <- shortest_paths(g,s,e)$vpath[[1]]

t1 <- c()
for(i in 1:nrow(walls)){
  ed <- edges
  if(walls[i,1]>1&walls[i,1]<141&walls[i,2]>1&walls[i,2]<141){
    n <- neigh(walls[i,])
    p <- paste(walls[i,1],walls[i,2],sep="_")
    np <- apply(n,1,\(x)paste(x[1],x[2],sep="_"))
    if(nrow(n)>0){
      ed <- rbind(ed,cbind(p,np),cbind(np,p))
    }
  }
  g <- graph_from_edgelist(ed)
  t1[i] <- length(shortest_paths(g,s,e)$vpath[[1]])
  cat(i,"\r") 
}
sum(t0-t1>=100)
#1393

d <- distances(g,v=e)
df <- reshape2::melt(d)
df <- df[df[,2]!=0,]
df[,1] <- as.character(df[,1])
df[,2] <- as.character(df[,2])

cb <- t(combn(1:9337,2))
save <- c()
for(i in 1:nrow(cb)){
  dist <- sum(abs(as.integer(strsplit(df[cb[i,1],2],"_")[[1]])-as.integer(strsplit(df[cb[i,2],2],"_")[[1]])))
  if(dist<=20){
    save[i] <- df$value[cb[i,1]]-df$value[cb[i,2]]-dist 
  }
  if(!i%%10000)cat(i,"\r")
}
sum(save>=100,na.rm=TRUE)

###990096
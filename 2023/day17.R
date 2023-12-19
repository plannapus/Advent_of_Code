source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(17)),""))
#input <- do.call(rbind,strsplit(readLines("test/test17.txt"),""))
map <- apply(input,2,as.integer)
start <- matrix(c(1,1),ncol=2)
end <- dim(map)

nextp <- function(current,d){
  if(d==1) return(current+c(0,1))
  if(d==2) return(current+c(0,-1))
  if(d==3) return(current+c(1,0))
  if(d==4) return(current+c(-1,0))
}

queue <- list(list(dir=c(),path=matrix(c(1,1),ncol=2), v=0))
dist <- expand.grid(1:nrow(map),1:nrow(map),1:4,1:3)
dist <- cbind(dist,Inf)
h <- 800
while(length(queue)){
  cur <- queue[[1]]
  dir <- cur$dir
  queue <- queue[-1]
  path <- cur$path
  v <- cur$v
  current <- path[nrow(path),]
  # points(current[2],current[1],pch=19,cex=0.5)
  directions <- 1:4
  lastdir <- tail(dir,1)
  posdir <- c(2:1,4:3)[!directions%in%lastdir]
  for(d in posdir){
    p <- nextp(current,d)
    r <- rle(c(dir,d))
    l <- tail(r$l,1)
    if(any(path[,1]==p[1]&path[,2]==p[2])) next
    if(l>3) next
    if(p[1]%in%c(0,nrow(map)+1)|p[2]%in%c(0,ncol(map)+1)) next
    newv <- v + map[p[1],p[2]]
    if(newv>h) next
    D <- abs(p[1]-end[1])+abs(p[2]-end[2])
    if(newv+D>h) next
    ind <- dist[,1]==p[1]&
      dist[,2]==p[2]&
      dist[,3]==d&
      dist[,4]==l
    if(newv<dist[ind,5]){
      dist[ind,5] <- newv
      if(p[1]==end[1]&p[2]==end[2]){
        if(newv<min(dist[dist[,1]==end[1]&dist[,2]==end[2],5])){
          pathend <- c(dir,d)
          h <- newv
        }
      }else{
          queue[[length(queue)+1]] <- list(dir=c(dir,d),v=newv,path=rbind(path,p))
      }
    }
  }
  queue <- queue[order(sapply(queue, \(x)x$v),decreasing=FALSE)]
  cat(length(queue),"-",max(dist[is.finite(dist[,5]),1]),",",max(dist[is.finite(dist[,5]),2]),"-",max(dist[is.finite(dist[,5]),5]),"-",min(dist[dist[,1]==end[1]&dist[,2]==end[2],5]),"\r")
}
cat("\n")
min(dist[dist[,1]==end[1]&dist[,2]==end[2],5])
cat("\n")
#684
write.table(dist,file="dist1.csv",sep="\t",row.names=FALSE)


edges <- data.frame(from=1,to=paste(5:11,1,"S",sep="_"),w=cumsum(map[2:11,1])[-(1:3)])
edges <- rbind(edges, data.frame(from=1,to=paste(1,5:11,"E",sep="_"),w=cumsum(map[1,2:11])[-(1:3)]))
while(any(!edges[,2]%in%edges[,1])){
  e <- unique(edges[!edges[,2]%in%edges[,1],2])
  #cat(length(e)," - ",nrow(edges),"\r")
  for(i in seq_along(e)){
    node <- strsplit(e[i],"_")[[1]]
    xy <- as.integer(node[1:2])
    d <- node[3]
    if(d%in%c("E","W")){
      N <- xy[1]-4:10
      S <- xy[1]+4:10
      N <- N[N>=1]
      S <- S[S<=nrow(map)]
      if(length(N)){
        nextnodes <- paste(N,xy[2],"N",sep="_")
        values <- cumsum(map[(xy[1]-1):min(N),xy[2]])[-(1:3)]
        edges <- rbind(edges,data.frame(from=e[i],to=nextnodes,w=values))
      }
      if(length(S)){
        nextnodes <- paste(S,xy[2],"S",sep="_")
        values <- cumsum(map[(xy[1]+1):max(S),xy[2]])[-(1:3)]
        edges <- rbind(edges,data.frame(from=e[i],to=nextnodes,w=values))
      }
    }else{
      W <- xy[2]-4:10
      E <- xy[2]+4:10
      W <- W[W>=1]
      E <- E[E<=ncol(map)]
      if(length(E)){
        nextnodes <- paste(xy[1],E,"E",sep="_")
        values <- cumsum(map[xy[1],(xy[2]+1):max(E)])[-(1:3)]
        edges <- rbind(edges,data.frame(from=e[i],to=nextnodes,w=values))
      }
      if(length(W)){
        nextnodes <- paste(xy[1],W,"W",sep="_")
        values <- cumsum(map[xy[1],(xy[2]-1):min(W)])[-(1:3)]
        edges <- rbind(edges,data.frame(from=e[i],to=nextnodes,w=values))
      }
    }
    cat(i,"/",length(e),"-",nrow(edges),"\r")
  }
}
write.table(edges,file="edges.csv",sep="\t",row.names=FALSE)
library(igraph)
g <- graph_from_data_frame(edges,directed=TRUE)
E(g)$weight <- as.integer(edges[,3])

pmin(distances(g,v="1",to="141_141_S"),distances(g,v="1",to="141_141_E"))

#No idea why but I need to recalculate, as the distance is not given weighted
all_shortest_paths(g,from="1",to="141_141_E",mode="out")->v1
n <- rep(0,length(v1$res))
for(j in seq_along(v1$res)){
  do.call(rbind,strsplit(names(v1$res[[j]]),"_"))->path
  cbind(as.integer(path[,1]),as.integer(path[,2]))->A
  for(i in 2:nrow(A)){
    if(A[i-1,1]>A[i,1]){
      x <- (A[i-1,1]-1):A[i,1]
    }else if(A[i-1,1]<A[i,1]){
      x <- (A[i-1,1]+1):A[i,1]
    }else{
      x <- A[i-1,1]
    }
    if(A[i-1,2]>A[i,2]){
      y <- (A[i-1,2]-1):A[i,2]
    }else if(A[i-1,2]<A[i,2]){
      y <- (A[i-1,2]+1):A[i,2]
    }else{
      y <- A[i-1,2]
    }
    n[j] <- n[j] + sum(map[x,y])
  }
}
n
#822

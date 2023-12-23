source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(23)),""))
#input <- do.call(rbind,strsplit(readLines("test/test23.txt"),""))
start <- c(1,2)
end <- c(141,140)
#start <- c(1,2)
#end <- c(23,22)

neighbours <- function(x,y){
  rbind(if(x<nrow(input)) if(!input[x+1,y]%in%c("^","#")&input[x,y]%in%c(".","v")) c(x+1,y),
        if(x>1) if(!input[x-1,y]%in%c("v","#")&input[x,y]%in%c(".","^")) c(x-1,y),
        if(y<ncol(input)) if(!input[x,y+1]%in%c("<","#")&input[x,y]%in%c(".",">")) c(x,y+1),
        if(y>1) if(!input[x,y-1]%in%c(">","#")&input[x,y]%in%c(".","<")) c(x,y-1))
}

path <- matrix(c(1,2),ncol=2)
queue <- list(path)
L<- c()
fullpaths <- list()
while(length(queue)){
  path <- queue[[1]]
  queue <- queue[-1]
  repeat{
    current <- path[nrow(path),]
    n <- neighbours(current[1],current[2])
    n <- n[!apply(n,1,\(x)any(path[,1]==x[1]&path[,2]==x[2])),,drop=FALSE]
    if(any(n[,1]==end[1]&n[,2]==end[2])){
      l <- nrow(path)
      L <- c(L,l)
      fullpaths <- c(fullpaths,list(rbind(path,n[n[,1]==end[1]&n[,2]==end[2],,drop=FALSE])))
      break
    }
    if(nrow(n)==1){
      path <- rbind(path,n)
    }else if(nrow(n)==0){
      break
    }else{
      for(i in 1:nrow(n)){
        queue <- c(queue,list(rbind(path,n[i,])))
      }
      break
    }
  }
}
max(L)
#2178

neighbours <- function(x,y){
  rbind(if(x<nrow(input))if(input[x+1,y]!="#")c(x+1,y),
        if(x>1)if(input[x-1,y]!="#")c(x-1,y),
        if(y<ncol(input))if(input[x,y+1]!="#")c(x,y+1),
        if(y>1)if(input[x,y-1]!="#")c(x,y-1))
}

edges <- data.frame(from_x=c(),from_y=c(),to_x=c(),to_y=c(),v=c())
path <- rbind(start)
nextp <- matrix(ncol=2,nrow=0)
repeat{
  if(nrow(path)){
    n <- neighbours(path[nrow(path),1],path[nrow(path),2])
    n <- n[!apply(n,1,\(x)any(path[,1]==x[1]&path[,2]==x[2])),,drop=FALSE]
    if(nrow(edges)) n <- n[!apply(n,1,\(x)any(edges[,3]==x[1]&edges[,4]==x[2])),,drop=FALSE]
    if(nrow(n)==1){
      path <- rbind(path,n)
    }else{
      edges <- rbind(edges,data.frame(from_x=path[1,1],from_y=path[1,2],
                                      to_x=path[nrow(path),1],to_y=path[nrow(path),2],
                                      v=nrow(path)-1))
      nextp <- rbind(nextp,n)
      path <- matrix(ncol=2,nrow=0)
    }
  }else{
    if(nrow(nextp)){
      path <- nextp[1,,drop=FALSE]
      nextp <- nextp[-1,,drop=FALSE]
      A <- apply(edges[,3:4],1,\(x)neighbours(x[1],x[2]),simplify=FALSE)
      w <- which(sapply(A,\(x)any(x[,1]==path[1]&x[,2]==path[2])))
      edges <- rbind(edges,data.frame(from_x=edges[w,3],from_y=edges[w,4],
                                      to_x=path[1,1],to_y=path[1,2],
                                      v=1))
    }else{
      break
    }
  }
}
library(igraph)
edges <- edges[edges$v!=0,]
e <- data.frame(from=paste(edges$from_x,edges$from_y,sep="_"),
                to=paste(edges$to_x,edges$to_y,sep="_"),
                v=edges$v)
g <- graph_from_data_frame(e,directed=FALSE)
start <- "1_2"
end <- "141_140"
#end <- "23_22"
asp <- all_simple_paths(g,from=start,to=end,mode="out")

path_length <- function(path){
  sum(sapply(2:length(path),\(i)c(e$v[e$from==path[i-1]&e$to==path[i]],e$v[e$to==path[i-1]&e$from==path[i]])))
}
a<-sapply(asp,\(x)path_length(names(x)))
max(a)
#6486
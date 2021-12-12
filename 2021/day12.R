#input <- read.table("test12_3.txt",sep="-")
input <- read.table("input12.txt",sep="-")
library(igraph)
g <- graph_from_data_frame(input,directed=FALSE)
v <- names(V(g))
V <- v[grepl("[A-Z]",v)]
for(i in V){
  n <- neighbors(g,i)
  new <- length(n)-1
  for(j in 1:new){
    g <- g + graph_from_data_frame(cbind(paste0(i,j),names(n)),directed=FALSE)
  }
}
asp <- all_simple_paths(g,"start","end")
s <- sapply(asp,function(x)names(x) <- gsub("[0-9]+","",names(x)))
sum(!duplicated(s))
#5756

# Part 2 (Works but too greedy)
s <- s[!duplicated(s)]
small <- v[grepl("[a-z]",v)]
small <- small[!small%in%c("start","end")]
for(i in seq_along(small)){
  h <- g
  n <- neighbors(h,small[i])
  h <- h + graph_from_data_frame(cbind(paste0(small[i],1),names(n)),directed=FALSE)
  asp <- all_simple_paths(h,"start","end")
  paths <- sapply(asp,function(x)names(x) <- gsub("[0-9]+","",names(x)))
  s <- c(s,paths)
  s <- s[!duplicated(s)]
  #cat(i,":",length(s),"\n")
}
length(s)

# Part 2 simpler:
g <- graph_from_data_frame(input,directed=FALSE)
v <- names(V(g))
V <- v[grepl("[A-Z]",v)]
small <- v[grepl("[a-z]",v)]
small <- small[!small%in%c("start","end")]
dfs <- function(node,visited,n){
  if(node=="end"){return(1)}
  if(node%in%visited){
    if(node%in%small & n<2){
      n <- 2
    }else{
      return(0)
    }
  }
  if(node%in%small|node=="start"){
    visited <- c(visited,node)
  }
  n_path <- 0
  for(child in names(neighbors(g,node))){
    n_path <- n_path + dfs(child,visited,n)
  }
  return(n_path)
}
dfs("start",c(),1)
# 144603
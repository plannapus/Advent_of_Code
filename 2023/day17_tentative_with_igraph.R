source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(17)),""))
#input <- do.call(rbind,strsplit(readLines("test/test17.txt"),""))
map <- apply(input,2,as.integer)
start <- matrix(c(1,1),ncol=2)
end <- dim(map)

nodes <- data.frame(i=1,j=1,d="",l=0,v=0)
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    if(i==1&j==1)next
    nodes <- rbind(nodes, 
                   data.frame(i=i,j=j,
                              d=rep(c("E","S","W","N"),each=3),
                              l=rep(1:3,4),
                              v=map[i,j]))
  }
  cat(i,"-",j,"\r")
}
nodes <- nodes[!(nodes$i==1&nodes$d=="S"),]
nodes <- nodes[!(nodes$i==141&nodes$d=="N"),]
nodes <- nodes[!(nodes$j==1&nodes$d=="E"),]
nodes <- nodes[!(nodes$j==141&nodes$d=="W"),]
nodes$n <- 1:nrow(nodes)

end_nodes <- nodes$n[nodes$i==141&nodes$j==141]

edges <- vector("list",length=nrow(nodes))
edges[[1]] <- rbind(c(1,2,2),c(1,1259,1))
for(i in 2:nrow(nodes)){
  edg <- matrix(ncol=3,nrow=0)
  n1 <- nodes$n[i]
  posdir <- c("W","E","S","N")[!c("E","W","N","S")%in%nodes$d[i]]
  if(nodes$l[i]==3) posdir <- posdir[!posdir%in%nodes$d[i]]
  nextnodes <- nodes[abs(nodes$i-nodes$i[i])==1&abs(nodes$j-nodes$j[i])==1&nodes$d%in%posdir,]
  for(j in 1:nrow(nextnodes)){
    if(nextnodes$d[j]==nodes$d[i]&nextnodes$l[j]==nodes$l[i]+1){
      n2 <- nextnodes$n[j]
      v <- nextnodes$v[j]
      edg <- rbind(edg, c(n1,n2,v))
    }
    if(nextnodes$d[j]!=nodes$d[i]&nextnodes$l[j]==1){
      n2 <- nextnodes$n[j]
      v <- nextnodes$v[j]
      edg <- rbind(edg, c(n1,n2,v))
    }
  }
  edges[[i]]<-edg
  cat(i,"\r")
}
e <- do.call(rbind,edges)
library(igraph)
g <- graph_from_edgelist(apply(e[,1:2],2,as.character),
                         directed=TRUE)
E(g)$weight <- e[,3]
DD <- distances(g,v="1")
DD[which(DD==684)]

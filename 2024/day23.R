library(igraph)
input <- do.call(rbind,strsplit(readLines(read.input(23)),"-"))
g <- graph_from_edgelist(input,directed=FALSE)
cl <- cliques(g,min=3,max=3)
sum(sapply(cl,\(x)any(grepl("^t",attr(x,"names")))))
#1423

paste(sort(attr(largest_cliques(g)[[1]],"names")),collapse=",")
#gt,ha,ir,jn,jq,kb,lr,lt,nl,oj,pp,qh,vy
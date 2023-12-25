input <- readLines(read.input(25))
s <- head(strsplit(input,": "),-1)
b <- sapply(s,\(x)strsplit(x[2]," "))
a <- sapply(s,\(x)x[1])
edges <- data.frame(from=c(),to=c())
for(i in seq_along(a)){
  for(j in seq_along(b[[i]])){
    edges <- rbind(edges,data.frame(from=a[i],to=b[[i]][j]))
  }
}
library(igraph)
g <- graph_from_data_frame(edges,directed=FALSE)
E(g)$label <- 1:nrow(edges)
plot(g)
#369,846,2029
h <- graph_from_data_frame(edges[-c(369,846,2029),],directed=FALSE)
distances(h)->d
prod(sapply(split(d[1,],is.finite(d[1,])),length))
#571753
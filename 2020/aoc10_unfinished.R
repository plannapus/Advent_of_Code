#Part 1
input <- scan("input10.txt")
chain <- c(0,sort(input),max(input)+3)
prod(table(diff(chain)))
#3000

#Part 2
library(igraph)
all_pairs <- combn(c(0,input,max(input)+3),2)
dist <- apply(all_pairs,2,function(x)abs(diff(x)))
tab <- cbind(t(all_pairs),dist)
vertices <- tab[tab[,3]%in%1:3,]
v <- t(apply(t(vertices[,1:2]),2,sort))
g <- graph_from_edgelist(v+1)
length(all_simple_paths(g,1,max(input)+1))

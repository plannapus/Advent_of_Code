#Day 18 Puzzle 1
library(igraph)
library(rgenoud)
options(digits=22)
input = readLines("input18.txt")
map = do.call(rbind,strsplit(input,""))
keys = t(sapply(letters,function(x)which(map==x,arr.ind=TRUE)))
doors = t(sapply(LETTERS,function(x)which(map==x,arr.ind=TRUE)))
me = which(map=="@",arr.ind=TRUE)
non_walkable = which(map=="#",arr.ind=TRUE)
all = expand.grid(1:81,1:81)
walkable = rbind(me,keys,doors)
colnames(all) = colnames(walkable)
for(i in 1:nrow(all)){
  if(!any(all[i,1]==walkable[,1]&all[i,2]==walkable[,2])){
    if(!any(all[i,1]==non_walkable[,1]&all[i,2]==non_walkable[,2])){
      walkable = rbind(walkable,all[i,])
    }
  }
}
eg = expand.grid(1:nrow(walkable),1:nrow(walkable))
ww = outer(1:nrow(walkable),1:nrow(walkable),function(x,y)abs(walkable[x,1]-walkable[y,1])+abs(walkable[x,2]-walkable[y,2]))
connected = which(ww==1,arr.ind=T)
key_vertices = apply(keys,1,function(x)which(walkable[,1]==x[1]&walkable[,2]==x[2]))
door_vertices = apply(doors,1,function(x)which(walkable[,1]==x[1]&walkable[,2]==x[2]))
g = graph_from_edgelist(connected,directed=FALSE)
dist=distances(g)

comb=as.data.frame(gtools::combinations(27,2,1:27))
comb$dist = apply(comb,1,function(x)dist[x[1],x[2]])
door_on_path = t(apply(comb,1,function(x)door_vertices%in%shortest_paths(g,x[1],x[2])$vpath[[1]]))

# 
# #Compute all possible paths
# step=cbind(1,key_vertices[sapply(key_vertices,function(x)!any(door_vertices%in%shortest_paths(g,1,x)$vpath[[1]]))])
# for(i in 1:26){
#   step = do.call(rbind,lapply(seq_len(nrow(step)),function(x){
#     path=step[x,]
#     not = key_vertices%in%path
#     other = key_vertices[!not]
#     do.call(rbind,lapply(other[sapply(other,function(y)!any(door_vertices[!not]%in%shortest_paths(g,tail(path,1),y)$vpath[[1]]))],function(z)c(path,z)))
#   }))
#   cat(i,":",nrow(step),"\r")
# }
# save(step,file="step.Rdata")
# 
# #Measure their distance
# dist=distances(g)
# d= sum(apply(embed(step,2),1,function(x)dist[x[1],x[2]]))
# cat("\n")
# cat(min(d))

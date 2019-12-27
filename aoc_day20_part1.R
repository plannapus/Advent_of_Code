#Day 20 Puzzle 1
library(igraph)
A=strsplit(readLines("input20.txt"),"")
A=do.call(rbind,A)
joinLetters = function(x){ 
  #function from mpjdem/adventofcode2019
  for(i in seq(length(x)-2)) {
    subv = x[i:(i+2)]
    b = paste(subv, collapse = "")
    if(nchar(b) > 3) next
    if(grepl("[A-Z]{2}\\.", b)) {
      x[i:(i+2)] = c(" ",paste(subv[1:2],collapse = ""), ".")
    }else if(grepl("\\.[A-Z]{2}", b)) {
      x[i:(i+2)] = c(".",paste(subv[2:3], collapse = ""), " ")
    }
  }
  x
}
A = t(apply(A, 1, joinLetters))
A = apply(A, 2, joinLetters)
walkable = which(A==".",arr.ind=T)
gates=A[grepl("[A-Z]",A)]
let = do.call(rbind,lapply(gates,function(x)cbind(x,which(A==x,arr.ind=T))))
let = let[!duplicated(let),]
gates=data.frame(gate=let[,1],x=as.integer(let[,2]),y=as.integer(let[,3]))
colnames(walkable)=c("x","y")
walkable=rbind(gates[,2:3],walkable)
ww = outer(1:nrow(walkable),1:nrow(walkable),function(x,y)abs(walkable[x,1]-walkable[y,1])+abs(walkable[x,2]-walkable[y,2]))
connected = which(ww==1,arr.ind=T)
tp = sapply(unique(gates$gate),function(x)which(gates$gate==x))
tp = do.call(rbind,tp[sapply(tp,length)==2])
for(i in 1:nrow(tp)){
  connected[connected==tp[i,1]] = tp[i,2]
}
connected = connected[connected[,1]!=connected[,2],]
g = graph_from_edgelist(connected,directed=FALSE)
sp=shortest_paths(g,which(gates$gate=="AA"),which(gates$gate=="ZZ"),output="vpath")
sum(!sp$vpath[[1]]%in%1:nrow(gates))-1
#400

#Day 20 Puzzle 2

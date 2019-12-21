#Day 20 Puzzle 1
library(igraph)
A<-strsplit(readLines("input20.txt"),"")
A=do.call(rbind,A)
walkable = which(A==".",arr.ind=T)
let = do.call(rbind,lapply(LETTERS,function(x)which(A==x,arr.ind=T)))
gates=do.call(rbind,apply(let,1,function(x){
  w = which(sapply(1:nrow(walkable),function(y)abs(x[1]-walkable[y,1])+abs(x[2]-walkable[y,2]))==1)
  nw = length(w)>0
  dif = walkable[w,]-x
  wens = c("W","E","N","S")
  wens = `if`(nw,wens,wens[c(2,1,4,3)])
  orient = wens[apply(matrix(c(0,1,0,-1,1,0,-1,0),ncol=2,byrow=T),1,function(y)y[1]==dif[1]&y[2]==dif[2])]
  nbs = t(apply(matrix(c(0,-1,1,0),nc=2,byrow=T),1,function(y)x+y))
  y = unlist(apply(nbs,1,function(x)let[let[,1]==x[1]&let[,2]==x[2],]))
  if(length(y)){
    a1 = A[x[1],x[2]]
    a2 = A[y[1],y[2]]
    gate = ifelse(orient%in%c("E","N"),
                  paste(a2,a1,sep=""),
                  paste(a1,a2,sep=""))
    return(data.frame(gate=gate,
                 x=ifelse(nw,x[1],y[1]),
                 y=ifelse(nw,x[2],y[2]),
                 stringsAsFactors=FALSE)[1,])
  }else{
    return(data.frame(gate=c(),x=c(),y=c()))
  }
  }))
colnames(walkable)<-c("x","y")
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

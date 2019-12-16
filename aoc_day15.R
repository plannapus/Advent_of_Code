#Day 15 Puzzle 1
library(igraph)
source("intcode_fast_but_dirty.R")
code = scan("input15.txt",sep=",")
direction = 1
A = list(op=code,n=1,rb=0,out=3)
pos = c(0,0)
past_pos = matrix(pos, ncol=2)
walls = c()
next_step = function(pos, direction, past_pos, walls){
  poss = t(apply(matrix(c(0,1,0,-1,-1,0,1,0),ncol=2,byrow=T),1,function(x)x+pos))
  nope = unlist(apply(past_pos,1,function(x)which(poss[,1]==x[1]&poss[,2]==x[2])))
  nopew = unlist(apply(walls,1,function(x)which(poss[,1]==x[1]&poss[,2]==x[2])))
  new_dir = 1:4
  # if(pos[1]==4 & pos[2]==20){
  #   direction=4
  # }else if(pos[1]==-9 & pos[2]==2){
  #   direction=1
  # }else{
    if(length(nope)) new_dir = new_dir[!new_dir%in%nope]
    if(length(nopew)) new_dir = new_dir[!new_dir%in%nopew]
    if(length(new_dir)){
      direction = sample(new_dir,1)
    }else{
      if(length(nopew)){
        new_dir = (1:4)[!(1:4)%in%nopew]
      }else{new_dir = 1:4}
      if(length(unique(nope))>1){
        direction = sample(new_dir,1,prob=1/table(factor(nope,new_dir)))
      }else{
        direction = nope[1]}
    }
  # }
  direction
}
explored = matrix(pos,ncol=2,nrow=1)
while(A$out){
  A = intfast(A$op,direction,n=A$n,rb=A$rb,m=1,
            verbose=FALSE,aggregate=FALSE)
  A$out = as.integer(A$out)
  if(!A$out){
    if(length(walls)){
      walls = rbind(walls,pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0)))
    }else{
      walls = matrix(pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0)),ncol=2)
    }
    explored = unique(rbind(explored,walls))
    direction = next_step(pos, direction, past_pos, walls)
  }else{
    pos = pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0))
    past_pos = rbind(past_pos,pos)
    past_pos = unique(past_pos)
    explored = unique(rbind(explored,past_pos))
    direction = next_step(pos, direction, past_pos, walls)
  }
  if(A$out==2){
    oxygen = pos
  }
}
w = unique(rbind(c(0,0),past_pos))
ox=which(w[,1]==oxygen[1]&w[,2]==oxygen[2])
W=apply(w,1,function(x)abs(w[,1]-x[1])+abs(w[,2]-x[2]))
z=which(W==1,arr.ind=T)
g=graph_from_edgelist(which(W==1,arr.ind=T))
d=distances(g,v=V(g),to=V(g))
d[1,ox]
#248

#Day 15 Puzzle 2
pp1 = unique(past_pos)
pw1 = unique(walls)
#pp1 = unique(rbind(past_pos,pp1))
#pw1 = unique(rbind(walls,pw1))
direction = 1
A = list(op=code,n=1,rb=0,out=3)
pos=c(0,0)
past_pos = matrix(pos, ncol=2)
walls = c()
plot(NA,xlim=c(-25,25),ylim=c(-25,25),pch=19,cex=2,an=F,ax=F)
points(unique(past_pos),col="grey",cex=2,pch=19)
points(pw1,cex=2,pch=19)
points(pp1,col="grey",cex=2,pch=19)
points(0,0,col="red",cex=2,pch=19)
points(oxygen[1],oxygen[2],col="blue",cex=2,pch=19)
repeat{ #Needs to be manually stopped
  A = intfast(A$op,direction,n=A$n,rb=A$rb,m=1,
              verbose=FALSE,aggregate=FALSE)
  A$out = as.integer(A$out)
  if(!A$out){
    if(length(walls)){
      walls = rbind(walls,pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0)))
      walls = unique(walls)
    }else{
      walls = matrix(pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0)),ncol=2)
    }
    points(walls,cex=2,pch=19)
    explored = unique(rbind(explored,walls))
    direction = next_step(pos, direction, past_pos, walls)
  }else{
    if(!any(past_pos[,1]==pos[1]&past_pos[,2]==pos[2]))points(pos[1],pos[2],col="grey80",cex=2,pch=19)
    pos = pos+switch(direction,"1"=c(0,1),"2"=c(0,-1),"3"=c(-1,0),"4"=c(1,0))
    past_pos = rbind(past_pos,pos)
    explored = unique(rbind(explored,past_pos))
    direction = next_step(pos, direction, past_pos, walls)
  }
  if(A$out==2){
    oxygen = pos
  }
  cat(nrow(explored),"\r")
  points(pos[1],pos[2],col="green",cex=1.5,pch=19)
}
w = unique(rbind(c(0,0),past_pos,pp1))
ox=which(w[,1]==oxygen[1]&w[,2]==oxygen[2])
W=apply(w,1,function(x)abs(w[,1]-x[1])+abs(w[,2]-x[2]))
z=which(W==1,arr.ind=T)
g=graph_from_edgelist(which(W==1,arr.ind=T))
d=distances(g,v=V(g),to=V(g))
max(d[,ox])
#382

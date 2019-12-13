#Day 13 Puzzle 1
source("intcode_fast_but_dirty.R")
code = scan("input13.txt",sep=",")
res=intfast(code,c(),1,1,0,FALSE,FALSE)
m=matrix(res$out,ncol=3,byrow=TRUE)
sum(m[,3]==2)
# 247

#Day 13 Puzzle 2
code2 = code
code2[1]=2
res=intfast(code2,c(),1,1,0,FALSE,FALSE)
m=matrix(res$out,ncol=3,byrow=TRUE)
n_blocks=sum(m[,3]==2)
while(n_blocks){
  ball = as.integer(m[m[,3]==4,1])
  paddle = as.integer(m[m[,3]==3,1])
  choice=sign(ball-paddle)
  res = intfast(res$op,choice,res$n,1,res$rb,FALSE,FALSE)
  m2=matrix(res$out,ncol=3,byrow=TRUE)
  for(i in 1:nrow(m2)){
    m[m[,1]==m2[i,1]&m[,2]==m2[i,2],3]=m2[i,3]
  }
  n_blocks=sum(m[,3]==2)
  cat(n_blocks,"\r")
}
cat(m[m[,1]==-1,3])
#12954

#####For fun#####
library(animation)
code2 = code
code2[1]=2
res=intfast(code2,c(),1,1,0,FALSE,FALSE)
m=matrix(res$out,ncol=3,byrow=TRUE)
n_blocks=sum(m[,3]==2)
read_out = function(m){
  M=apply(m,2,as.integer)
  map = matrix(0,nr=37,nc=22)
  score = M[M[,1]==-1,3]
  M = M[M[,1]>=0,]
  for(i in 1:nrow(M)){
    map[M[i,1]+1,M[i,2]+1]=M[i,3]
  }
  list(map=map,score=score)
}
saveGIF({
while(n_blocks){
  ball = as.integer(m[m[,3]==4,1])
  paddle = as.integer(m[m[,3]==3,1])
  choice=sign(ball-paddle)
  res = intfast(res$op,choice,res$n,1,res$rb,FALSE,FALSE)
  m2=matrix(res$out,ncol=3,byrow=TRUE)
  for(i in 1:nrow(m2)){
    m[m[,1]==m2[i,1]&m[,2]==m2[i,2],3]=m2[i,3]
  }
  n_blocks=sum(m[,3]==2)
  rm = read_out(m)
  image(rm$map,ann=FALSE,axes=FALSE,ylim=c(23,0),xlim=c(0,38),xaxs="i",yaxs="i",col=c("white","black","blue","red"),xlab=rm$score)
}
},"aoc13.gif")

#Day 11 Puzzle 1
source("intcode_fast_but_dirty.R")
code = scan("input11.txt",sep=",")
backup=code
pos=matrix(c(0,0,0),nr=1)
dir="up"
dirs = c("up","left","down","right")
i=1
A = list(n=1,rb=0,status=1,op=code)
while(A$status){
  A = intfast(A$op,pos[i,3],A$n,1,A$rb)
  pos[i,3]=as.integer(substr(A$out,1,1))
  newdir=as.integer(substr(A$out,2,2))
  w=which(dirs%in%dir)+ifelse(newdir,-1,1)
  if(!w%in%1:4) w = w%%4
  if(!w) w = 4
  dir=dirs[w]
  newpos = pos[i,1:2]+switch(dir,"up"=c(0,1),"left"=c(-1,0),"down"=c(0,-1),"right"=c(1,0))
  pp = pos[newpos[1]==pos[,1]&newpos[2]==pos[,2],]
  if(length(pp)){
    col = ifelse(length(pp)>3, tail(pp[,3],1), pp[3])
    newpos = c(newpos,col)
  }else{
    newpos = c(newpos,0)
  }
  pos = rbind(pos,newpos)
  i=i+1
  if(!i%%100)cat(i,"\r")
}
nrow(unique(pos[-nrow(pos),1:2]))
#1907

#Day 11 Puzzle 2
pos=matrix(c(0,0,1),nr=1)
A = list(n=1,rb=0,status=1,op=backup)
i=1
dir="up"
while(A$status){
  A = intfast(A$op,pos[i,3],A$n,1,A$rb)
  pos[i,3]=as.integer(substr(A$out,1,1))
  newdir=as.integer(substr(A$out,2,2))
  w=which(dirs%in%dir)+ifelse(newdir,-1,1)
  if(!w%in%1:4) w = w%%4
  if(!w) w = 4
  dir=dirs[w]
  newpos = pos[i,1:2]+switch(dir,"up"=c(0,1),"left"=c(-1,0),"down"=c(0,-1),"right"=c(1,0))
  pp = pos[newpos[1]==pos[,1]&newpos[2]==pos[,2],]
  if(length(pp)){
    col = ifelse(length(pp)>3, tail(pp[,3],1), pp[3])
    newpos = c(newpos,col)
  }else{
    newpos = c(newpos,0)
  }
  pos = rbind(pos,newpos)
  i=i+1
  if(!i%%100)cat(i,"\r")
}
m=matrix(nrow=43,ncol=6)
for(i in 1:nrow(pos)){
  m[pos[i,1]+1,abs(pos[i,2])+1]=pos[i,3]
}
image(0:42,0:5,m,col=c("black","white"),ylim=c(6,-1),xlim=c(-1,43))
#ABEKZBFG

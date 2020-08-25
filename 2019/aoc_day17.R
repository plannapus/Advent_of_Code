#Day 17 Puzzle 1
source("intcode_fast_but_dirty.R")
code=scan("input17.txt",sep=",")
A=intfast(code,c(),1,1,0,FALSE,FALSE)
ascii=rawToChar(as.raw(A$out))
map=do.call(rbind,strsplit(strsplit(ascii,"\n")[[1]],""))
w=which(map=="#",arr.ind=T)
intersections = c()
for(i in 1:nrow(w)){
  neighbours = t(apply(matrix(c(-1,0,1,0,0,1,0,-1),nrow=2),2,function(x)x+w[i,]))
  neigh_in_list = apply(neighbours,1,function(x)any(x[1]==w[,1]&x[2]==w[,2]))
  if(all(neigh_in_list)){
    if(length(intersections)){
      intersections = rbind(intersections,w[i,])
    }else{
      intersections=w[i,]
    }
  }
}
sum(apply(intersections-1,1,prod))
#7584

#Day 17 Puzzle 2
code[1]=2
snew = matrix(c(1,0,-1,0,0,1,0,-1),ncol=2,byrow=T)
dirFromCoord = function(from,to){
  dif = to-from
  c("S","N","E","W")[apply(snew,1,function(x)x[1]==dif[1]&x[2]==dif[2])]
}
robot=which(map=="^",arr.ind=T)
neighbours = t(apply(t(snew),2,function(x)x+robot))
neigh_in_list = neighbours[apply(neighbours,1,function(x)any(x[1]==w[,1]&x[2]==w[,2])),]
dirs = dirFromCoord(robot,neigh_in_list)
visited = matrix(neigh_in_list,ncol=2)

while(nrow(unique(visited))!=nrow(w)){
  lastdir = tail(dirs,1)
  next_in_line = visited[nrow(visited),]+snew[c("S","N","E","W")%in%lastdir,]
  nil = next_in_line[1]==w[,1]&next_in_line[2]==w[,2]
  if(any(nil)){
    visited = rbind(visited,w[nil,])
    dirs = c(dirs,lastdir)
  }else{
    neighbours = t(apply(t(snew),2,function(x)x+visited[nrow(visited),]))
    neigh_in_list = neighbours[apply(neighbours,1,function(x)any(x[1]==w[,1]&x[2]==w[,2])&!any(x[1]==visited[,1]&x[2]==visited[,2])),]
    nextdir = dirFromCoord(visited[nrow(visited),],neigh_in_list)
    dirs = c(dirs,nextdir)
    visited = rbind(visited,neigh_in_list)
  }
}
rd=rle(dirs)
dd=c("N",rd$values)
dirchange=apply(embed(dd,2)[,2:1],1,function(x){
  d=rep(c("W","N","E","S"),2)
  e1=head(which(d==x[1]),1)
  e2=tail(which(d==x[2]),1)
  e3=(e2-e1)%%4
  ifelse(e3==1,"R",ifelse(e3==3,"L",""))
})
totalpath=paste(dirchange,rd$length,sep=",")
# el=unique(totalpath)
# tp=totalpath
# for(i in 1:5)tp[tp==el[i]]=i
# tp<-paste(tp,collapse="")
# tp
# #"123441241245345312344124124531234"
# gsub("1234","A",tp)->tp
# tp
# #"A412412453453A412412453A"
# gsub("412","B",tp)->tp
# #"ABB453453ABB453A"
# gsub("453","C",tp)->tp
# tp
# #"ABBCCABBCA"
A="R,4,R,12,R,10,L,12"
B="L,12,R,4,R,12"
C="L,12,L,8,R,10"
totalpath=gsub(A,"A",paste(totalpath,collapse=","))
totalpath=gsub(B,"B",totalpath)
totalpath=gsub(C,"C",totalpath)
cmd1=as.integer(charToRaw(paste(totalpath,"\n",sep="")))
cmd2=as.integer(charToRaw(paste(A,"\n",sep="")))
cmd3=as.integer(charToRaw(paste(B,"\n",sep="")))
cmd4=as.integer(charToRaw(paste(C,"\n",sep="")))
cmd5=as.integer(charToRaw("n\n"))
D=intfast(code,c(),1,1,0,FALSE,FALSE)
D=intfast(D$op,cmd1,D$n,1,D$rb,FALSE,FALSE)
D=intfast(D$op,cmd2,D$n,1,D$rb,FALSE,FALSE)
D=intfast(D$op,cmd3,D$n,1,D$rb,FALSE,FALSE)
D=intfast(D$op,cmd4,D$n,1,D$rb,FALSE,FALSE)
D=intfast(D$op,cmd5,D$n,1,D$rb,FALSE,FALSE)
cat(tail(D$out,1))
#1016738

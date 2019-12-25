#Day 23 Puzzle 1
options(digits=22)
source("intcode_fast_but_dirty.R")
code = scan("input23.txt",sep=",")
A=list()
for(i in 1:50){
  A[[i]] = intfast(code,i-1,1,1,0,FALSE,FALSE)
}
output=matrix(nrow=0,ncol=3)
step=0
while(all(sapply(A,function(x)x$status)!=0)){
  for(i in 1:50){
    if(nrow(output)&ncol(output)){
      if(i%in%output[,1]+1){
        input = output[as.integer(output[,1])+1==i,2:3,drop=FALSE]
        output = output[as.integer(output[,1])+1!=i,,drop=FALSE]
      }else{
        input = matrix(-1,nrow=1)
      }
    }else{
      input = matrix(-1,nrow=1)
    }
    for(j in seq_len(nrow(input))){
      A[[i]] = intfast(A[[i]]$op,as.numeric(input[j,]),A[[i]]$n,1,A[[i]]$rb,FALSE,FALSE)
      if(length(A[[i]]$out)) output = rbind(output,matrix(A[[i]]$out,ncol=3,byrow=TRUE))
    }
  }
  step = step + 1
  cat(step,"\r")
  if(any(output[,1]==255)){
    stop(sprintf("Packet for address 255: (%s,%s)\n",output[output[,1]==255,2],output[output[,1]==255,3]))
  }
}
#Error: Packet for address 255: (53891,24602)

#Day 23 Puzzle 2
memory_nat = c(output[output[,1]==255,2],output[output[,1]==255,3])
output = output[output[,1]!=255,,drop=FALSE]
sent=c()
while(all(sapply(A,function(x)x$status)!=0)){
  for(i in 1:50){
    if(nrow(output)&ncol(output)){
      if(i%in%output[,1]+1){
        input = output[as.integer(output[,1])+1==i,2:3,drop=FALSE]
        output = output[as.integer(output[,1])+1!=i,,drop=FALSE]
      }else{
        input = matrix(-1,nrow=1)
      }
    }else{
      input = matrix(-1,nrow=1)
    }
    for(j in seq_len(nrow(input))){
      A[[i]] = intfast(A[[i]]$op,as.numeric(input[j,]),A[[i]]$n,1,A[[i]]$rb,FALSE,FALSE)
      if(length(A[[i]]$out)){
        new = matrix(A[[i]]$out,ncol=3,byrow=TRUE)
        output = rbind(output,new)
        if(255%in%new[,1]){
          memory_nat = tail(new[new[,1]==255,2:3,drop=FALSE],1)
          output = output[output[,1]!=255,,drop=FALSE]
          }
      }
    }
  }
  step = step + 1
  cat(step,"\r")
  idle = !nrow(output)&!all(sapply(A,function(x)x$status)==0)
  if(idle){
    output = matrix(c(0,memory_nat[1],memory_nat[2]),ncol=3)
    if(length(sent)){
      sent=rbind(sent,memory_nat)
    }else{sent = matrix(memory_nat,ncol=2)}
    if(any(duplicated(apply(sent,1,paste,collapse="_")))) stop(sprintf("First package sent twice: (%s,%s)\n",memory_nat[1],memory_nat[2]))
  }
}
#Error: First package sent twice: (53891,19641)
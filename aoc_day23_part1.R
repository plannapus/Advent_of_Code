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

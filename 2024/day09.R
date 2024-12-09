input <- readLines(read.input(9))
input <- as.integer(strsplit(input,"")[[1]])
#input <- as.integer(strsplit("2333133121414131402","")[[1]])
tape <- c()
n<-0
j=1
for(i in seq_along(input)){
  if(i%%2==1){
    tape[j:(j+input[i]-1)] <- n
    n <- n+1
    j <- j+input[i]
  }else{
    if(input[i]!=0){
      tape[j:(j+input[i]-1)] <- NA
      j <- j+input[i]
    }
  }
}

tape2 <- tape

repeat{ #yes it is slow
  na <- which(is.na(tape))
  a <- which(!is.na(tape))
  if(length(na)==0) break
  tape[head(na,1)]<-tape[tail(a,1)]
  tape <- tape[-tail(a,1)]
  cat(length(na),"\r")
}
options(digits=22)
sum(tape*(seq_along(tape)-1))
#6421128769094

tape <- tape2
tape[is.na(tape)]<--1
mapping <- cbind(0:max(tape),input[(seq_along(input)%%2)==1])
for(i in nrow(mapping):1){
  r <- rle(tape)
  m <- mapping[i,2]
  nw <- which(tape==mapping[i,1])
  w <- which(r$l>=m & r$v==-1)
  if(length(w)){
    j <- sum(r$l[1:(w[1]-1)])+1
    if(j<=nw[1]){
      tape[j:(j+m-1)] <- mapping[i,1]
      tape[nw] <- -1 
    }
  }
  cat(i,"\r")
}
tape[tape==-1] <- NA
sum(tape*(seq_along(tape)-1),na.rm=TRUE)
#6448168620520
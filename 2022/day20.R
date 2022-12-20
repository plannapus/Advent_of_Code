input <- scan("input20.txt")
#input <- c(1, 2, -3, 3, -2, 0, 4)
input <- cbind(1:length(input),input)
for(i in seq_len(nrow(input))){
  w <- which(input[,1]==i)
  v <- input[w,2]
  n <- (w+v)
  while(n<=0){
    n <- n+nrow(input)-1
  }
  while(n>nrow(input)){
    n <- n-nrow(input)+1
  }
  input <- input[-w,]
  if(n>nrow(input)){
    input <- rbind(input,c(i,v))
  }else{
    input <- rbind(input[seq_len(n-1),],c(i,v),input[n:nrow(input),])
  }
}

a <- which(input[,2]==0)
sum(input[(a+c(1000,2000,3000))%%nrow(input),2])
#4066

options(digits=22)
input <- scan("input20.txt")
#input <- c(1, 2, -3, 3, -2, 0, 4)
input <- cbind(1:length(input),input*811589153)
indices <- 0:(nrow(input)-1)
for(k in 1:10){
  for(i in seq_len(nrow(input))){
    v <- input[input[,1]==i,2]
    w <- indices[i]
    n <- (w+v)%%(nrow(input)-1)
    indices[indices>w] <- (indices[indices>w]-1)%%nrow(input)
    indices[indices>=n] <- (indices[indices>=n]+1)%%nrow(input)
    indices[i] <- n
    #cat(k,":",i,"\r")
  }
}
a <- indices[input[,2]==0]
sum(input[which(indices%in%((a+c(1000,2000,3000))%%nrow(input))),2])
#6704537992933

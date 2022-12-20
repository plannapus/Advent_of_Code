input <- scan("input20.txt")
#input <- c(1, 2, -3, 3, -2, 0, 4)
input <- cbind(1:length(input),input)
#for(i in 1:3092){
for(i in seq_len(nrow(input))){
  w <- which(input[,1]==i)
  v <- input[w,2]
  n <- (w+v)
  if(n>nrow(input)){
    n <- n%%nrow(input)+2
  }else if(n<=1){
    n <- (n%%nrow(input))-1
  }
  input <- input[-w,]
  if(n==0|n==nrow(input)){
    input <- rbind(input,c(i,v))
  }else if(n==-1){
    input <- rbind(input[1:(nrow(input)-1),],c(i,v),input[nrow(input),])
  }else{
    input <- rbind(input[seq_len(n-1),],c(i,v),input[n:nrow(input),])
  }
  #cat(input[,2],sep=" ")
  #cat("\n")
}

a <- which(input[,2]==0)
sum(input[(a+c(1000,2000,3000))%%nrow(input),2])

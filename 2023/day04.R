input <- readLines(read.input(4))
input <- strsplit(input,"[:|]")
winning <- lapply(input,\(x)as.integer(el(strsplit(x[2]," +")))[-1])
got <- lapply(input,\(x)as.integer(el(strsplit(x[3]," +")))[-1])
n <- 0
for(i in 1:length(winning)){
  n <- n + floor(2 ^ (sum(got[[i]]%in%winning[[i]])-1))
}
#25651

ncards <- rep(1,203)
for(i in 1:203){
  n <- sum(got[[i]]%in%winning[[i]])
  if(n>0) ncards[(i+1):(i+n)] <- ncards[(i+1):(i+n)]+ncards[i]
}
sum(ncards)
#19499881
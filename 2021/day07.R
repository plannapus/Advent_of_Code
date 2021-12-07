# Part 1
input <- scan("input07.txt",sep=",")
r <- range(input)
d <- c()
for(i in r[1]:r[2]){
  d[i] <- sum(abs(input-i))
}
min(d,na.rm=TRUE)
#328318

# Part 2
d <- c()
for(i in r[1]:r[2]){
  dis <- abs(input-i)
  d[i] <- sum(sapply(dis,function(x)sum(seq_len(x))))
}
min(d,na.rm=TRUE)
#89791146
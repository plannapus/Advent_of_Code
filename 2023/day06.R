input <- readLines(read.input(6))
time <- as.integer(strsplit(gsub("Time: +","",input[1])," +")[[1]])
distance <- as.integer(strsplit(gsub("Distance: +","",input[2])," +")[[1]])
poss <- c()
for(i in seq_along(time)){
  a <- 1:time[i]
  poss[i] <- sum((time[i]-a)*a>distance[i])
}
prod(poss)
#3317888

options(digits=22)
time2 <- as.integer(paste(time,collapse=""))
distance2 <- as.numeric(paste(distance,collapse=""))
a <- as.numeric(1:time2)
sum((time2-a)*a>distance2)
#24655068

##Alternative part 2
time2 <- as.integer(paste(time,collapse=""))
distance2 <- as.numeric(paste(distance,collapse=""))
floor((time2+sqrt(time2^2-4*distance2))/2) - ceiling((time2-sqrt(time2^2-4*distance2))/2) + 1
#24655068

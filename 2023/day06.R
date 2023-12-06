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
time <- as.integer(paste(time,collapse=""))
distance <- as.numeric(paste(distance,collapse=""))
a <- as.numeric(1:time)
sum((time-a)*a>distance)
#24655068
input <- readLines("input13.txt")
ts <- as.integer(input[1])
bus <- as.integer(el(strsplit(input[2],",")))
bus <- bus[!is.na(bus)]
best <- bus[which.min(bus - ts%%bus)]
best * (best - ts%%best)
#153

options(digits=22)
bus <- as.integer(el(strsplit(input[2],",")))
spot <- which(!is.na(bus))-1
bus <- bus[!is.na(bus)]
t <-0
pace <- 1
for(i in seq_along(bus)){
  while((t+spot[i])%%bus[i]) t <- t+pace
  pace <- pace*bus[i]
}
t
#471793476184394
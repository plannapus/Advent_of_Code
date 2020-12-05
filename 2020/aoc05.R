## Part 1
plane_map <- matrix("",nrow=128,ncol=8)
for(i in 6:0) plane_map <- apply(plane_map,2,function(x)paste(x,c(rep("F",2^i),rep("B",2^i)),sep=""))
for(i in 2:0) plane_map <- t(apply(plane_map,1,function(x)paste(x,c(rep("L",2^i),rep("R",2^i)),sep="")))
seat_id <- (row(plane_map)-1)*8+(col(plane_map)-1)

input <- readLines("input05.txt")
seats_taken <- sapply(input,function(x)seat_id[plane_map==x])
max(seats_taken)
#933

##Part2
remains <- (0:1023)[!(0:1023)%in%seats_taken]
remains[!(remains-1)%in%remains & !(remains+1)%in%remains]
#711

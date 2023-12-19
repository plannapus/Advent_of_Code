input <- read.table(read.input(18),sep=" ",header=FALSE, comment.char="")
start <- path <- matrix(c(0,0),ncol=2)
for(i in 1:nrow(input)){
  if(input[i,1]=="R") add <- c(0,1)
  if(input[i,1]=="D") add <- c(1,0)
  if(input[i,1]=="L") add <- c(0,-1)
  if(input[i,1]=="U") add <- c(-1,0)
  for(j in seq_len(as.integer(input[i,2]))){
    path <- rbind(path,path[nrow(path),]+add)
  }
}
(abs(sum(path[-nrow(path),1]*path[-1,2])-sum(path[-nrow(path),2]*path[-1,1]))+nrow(path)-1)/2+1
#50465

g <- gsub("[)(#)]","",input[,3])
l <- strtoi(substr(g,1,5),16)
d <- c("R","D","L","U")[as.integer(substr(g,6,6))+1]
start <- path <- matrix(c(0,0),ncol=2)
for(i in 1:nrow(input)){
  if(d[i]=="R") add <- c(0,1)
  if(d[i]=="D") add <- c(1,0)
  if(d[i]=="L") add <- c(0,-1)
  if(d[i]=="U") add <- c(-1,0)
  path <- rbind(path,path[nrow(path),]+add*l[i])
}
options(digits=22)
area <- abs(sum(path[-nrow(path),1]*path[-1,2])-sum(path[-nrow(path),2]*path[-1,1]))/2
perim <- sum(abs(diff(path[,1])+diff(path[,2])))
area+perim/2+1
#82712746433310
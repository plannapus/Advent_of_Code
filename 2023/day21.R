source("read.input.R")
options(digits=22)
input <- do.call(rbind,strsplit(readLines(read.input(21)),""))
neighbours <- function(x,y){
  n <- rbind(c(x+1,y),c(x-1,y),c(x,y+1),c(x,y-1))
  n <- n[n[,1]>0&n[,1]<=nrow(input)&n[,2]>0&n[,2]<=ncol(input),]
  w <- apply(n,1,\(x)input[x[1],x[2]])
  n[w!="#",,drop=FALSE]
}
pos <- which(input=="S",arr.ind=TRUE)
res <- list()
res[[1]] <- pos
for(i in 1:64){
  n <- c()
  for(j in 1:nrow(res[[i]])){
    n <- rbind(n,neighbours(res[[i]][j,1],res[[i]][j,2]))
  }
  n <- n[!duplicated(n),]
  res[[i+1]] <- n
  cat(i,nrow(res[[i+1]]),"\r")
}
nrow(res[[65]])
#3578

steps <- 26501365

neighbours <- function(x,y){
  n <- rbind(c(x+1,y),c(x-1,y),c(x,y+1),c(x,y-1))
  w <- apply(n,1,\(x){
    if(x[1]==0) x[1] <- 131
    if(x[2]==0) x[2] <- 131
    if(x[1]<0) x[1] <- x[1]%%131
    if(x[2]<0) x[2] <- x[2]%%131
    if(x[1]>131) x[1] <- x[1]%%131
    if(x[2]>131) x[2] <- x[2]%%131
    input[x[1],x[2]]
    })
  n[w!="#",,drop=FALSE]
}
pos <- which(input=="S",arr.ind=TRUE)
res <- list()
res[[1]] <- pos
a <- c()
for(i in 1:steps){
  n <- do.call(rbind,apply(res[[i]],1,\(x)neighbours(x[1],x[2]),simplify=FALSE))
  n <- n[!duplicated(n),]
  res[[i+1]] <- n
  if(i%%131==steps%%131){a <- c(a,nrow(res[[i+1]]))}
  if(length(a)==3) break
  cat(i,nrow(res[[i+1]]),"\r")
}
save(res,a,file="save21.Rdata")
b <- c(a[1],a[2]-a[1],a[3]-a[2])
x <- steps%/%131
b[1]+b[2]*x+((x*(x-1))%/%2)*(b[3]-b[2])
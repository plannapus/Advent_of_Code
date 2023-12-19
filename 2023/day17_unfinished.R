source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(17)),""))
#input <- do.call(rbind,strsplit(readLines("test/test17.txt"),""))
map <- apply(input,2,as.integer)
start <- matrix(c(1,1),ncol=2)
end <- dim(map)

nextp <- function(current,d){
  if(d==1) return(current+c(0,1))
  if(d==2) return(current+c(0,-1))
  if(d==3) return(current+c(1,0))
  if(d==4) return(current+c(-1,0))
}

# d2path <- function(dir){
#   path <- matrix(c(1,1),ncol=2)
#   for(i in seq_along(dir)) path <- rbind(path, nextp(path[nrow(path),],dir[i]))
#   path
# }
# 
# p2v <- function(path){
#   v <- 0
#   if(nrow(path)==1) return(0)
#   for(i in 2:nrow(path)){
#     v <- v + map[path[i,1],path[i,2]]
#   }
#   return(v)
# }

# par(mar=c(0,0,0,0))
# plot(NA,xlim=c(1,nrow(map)),ylim=c(ncol(map),1),ax=F,ann=F)
queue <- list(list(dir=c(),path=matrix(c(1,1),ncol=2), v=0))
dist <- expand.grid(1:nrow(map),1:nrow(map),1:4,1:3)
dist <- cbind(dist,Inf)
h <- 800
while(length(queue)){
  cur <- queue[[1]]
  dir <- cur$dir
  queue <- queue[-1]
  path <- cur$path
  v <- cur$v
  current <- path[nrow(path),]
  # points(current[2],current[1],pch=19,cex=0.5)
  directions <- 1:4
  lastdir <- tail(dir,1)
  posdir <- c(2:1,4:3)[!directions%in%lastdir]
  for(d in posdir){
    p <- nextp(current,d)
    r <- rle(c(dir,d))
    l <- tail(r$l,1)
    if(any(path[,1]==p[1]&path[,2]==p[2])) next
    if(l>3) next
    if(p[1]%in%c(0,nrow(map)+1)|p[2]%in%c(0,ncol(map)+1)) next
    newv <- v + map[p[1],p[2]]
    if(newv>h) next
    D <- abs(p[1]-end[1])+abs(p[2]-end[2])
    if(newv+D>h) next
    ind <- dist[,1]==p[1]&
      dist[,2]==p[2]&
      dist[,3]==d&
      dist[,4]==l
    if(newv<dist[ind,5]){
      dist[ind,5] <- newv
      if(p[1]==end[1]&p[2]==end[2]){
        if(newv<min(dist[dist[,1]==end[1]&dist[,2]==end[2],5])){
          pathend <- c(dir,d)
          h <- newv
        }
      }else{
          queue[[length(queue)+1]] <- list(dir=c(dir,d),v=newv,path=rbind(path,p))
      }
    }
  }
  queue <- queue[order(sapply(queue, \(x)x$v),decreasing=FALSE)]
  cat(length(queue),"-",max(dist[is.finite(dist[,5]),1]),",",max(dist[is.finite(dist[,5]),2]),"-",max(dist[is.finite(dist[,5]),5]),"-",min(dist[dist[,1]==end[1]&dist[,2]==end[2],5]),"\r")
}
cat("\n")
min(dist[dist[,1]==end[1]&dist[,2]==end[2],5])
cat("\n")
#684
write.table(dist,file="dist1.csv",sep="\t",row.names=FALSE)

queue <- list(list(dir=c(),path=matrix(c(1,1),ncol=2), v=0))
dist <- expand.grid(1:nrow(map),1:nrow(map),1:4,1:10)
dist <- cbind(dist,Inf)
h <- 1000
while(length(queue)){
  cur <- queue[[1]]
  dir <- cur$dir
  queue <- queue[-1]
  path <- cur$path
  v <- cur$v
  current <- path[nrow(path),]
  # points(current[2],current[1],pch=19,cex=0.5)
  directions <- 1:4
  lastdir <- tail(dir,1)
  posdir <- c(2:1,4:3)[!directions%in%lastdir]
  for(d in posdir){
    p <- nextp(current,d)
    r <- rle(c(dir,d))
    l <- tail(r$l,1)
    if(any(path[,1]==p[1]&path[,2]==p[2])) next
    if(l>10) next
    if(l<4){
      if(length(r$l)>1){
        if(tail(r$l,2)[1]<4) next
      }
    }
    if(p[1]%in%c(0,nrow(map)+1)|p[2]%in%c(0,ncol(map)+1)) next
    newv <- v + map[p[1],p[2]]
    if(newv>h) next
    D <- abs(p[1]-end[1])+abs(p[2]-end[2])
    if(newv+D>h) next
    ind <- dist[,1]==p[1]&
      dist[,2]==p[2]&
      dist[,3]==d&
      dist[,4]==l
    if(newv<dist[ind,5]){
      dist[ind,5] <- newv
      if(p[1]==end[1]&p[2]==end[2]){
        if(newv<min(dist[dist[,1]==end[1]&dist[,2]==end[2],5])){
          pathend <- c(dir,d)
          h <- newv
        }
      }else{
        queue[[length(queue)+1]] <- list(dir=c(dir,d),v=newv,path=rbind(path,p))
      }
    }
  }
  queue <- queue[order(sapply(queue, \(x)x$v),decreasing=FALSE)]
  cat(length(queue),"-",max(dist[is.finite(dist[,5]),1]),",",max(dist[is.finite(dist[,5]),2]),"-",max(dist[is.finite(dist[,5]),5]),"-",min(dist[dist[,1]==end[1]&dist[,2]==end[2],5]),"\r")
}
min(dist[dist[,1]==nrow(map)&dist[,2]==ncol(map),5])
write.table(dist,file="dist2.csv",sep="\t",row.names=FALSE)

source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(17)),""))
#input <- do.call(rbind,strsplit(readLines("test/test17.txt"),""))
map <- apply(input,2,as.integer)
start <- matrix(c(1,1),ncol=2)

nextp <- function(current,d){
  if(d==1) return(current+c(0,1))
  if(d==2) return(current+c(0,-1))
  if(d==3) return(current+c(1,0))
  if(d==4) return(current+c(-1,0))
}

d2path <- function(dir){
  path <- matrix(c(1,1),ncol=2)
  for(i in seq_along(dir)) path <- rbind(path, nextp(path[nrow(path),],dir[i]))
  path
}

p2v <- function(path){
  v <- 0
  if(nrow(path)==1) return(0)
  for(i in 2:nrow(path)){
    v <- v + map[path[i,1],path[i,2]]
  }
  return(v)
}

# par(mar=c(0,0,0,0))
# plot(NA,xlim=c(1,nrow(map)),ylim=c(ncol(map),1),ax=F,ann=F)
queue <- list(c())
dist <- expand.grid(1:nrow(map),1:nrow(map),1:4,1:3)
dist <- cbind(dist,Inf)
path <- c()
h <- 700
while(length(queue)){
  dir <- queue[[1]]
  queue <- queue[-1]
  path <- d2path(dir)
  v <- p2v(path)
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
    ind <- dist[,1]==p[1]&
      dist[,2]==p[2]&
      dist[,3]==d&
      dist[,4]==l
    if(newv<dist[ind,5]){
      dist[ind,5] <- newv
      if(p[1]==nrow(map)&p[2]==ncol(map)){
        if(newv<min(dist[dist[,1]==nrow(map)&dist[,2]==ncol(map),5])){
          path <- c(dir,d)
          h <- newv
        }
      }else{
          queue[[length(queue)+1]] <- c(dir,d)
      }
    }
  }
  queue <- queue[order(sapply(queue, \(x)p2v(d2path(x))),decreasing=TRUE)]
  cat(length(queue),"-",max(dist[is.finite(dist[,5]),1]),",",max(dist[is.finite(dist[,5]),2]),"-",min(dist[dist[,1]==nrow(map)&dist[,2]==nrow(map),5]),"\r")
}
cat("\n")
min(dist[dist[,1]==nrow(map)&dist[,1]==ncol(map),5])
cat("\n")
write.table(dist,file="dist1.csv",sep="\t",row.names=FALSE)

queue <- list(list(v=0, current=start, dir=c(), path=start))
dist <- expand.grid(1:nrow(map),1:nrow(map),1:10,1:3)
dist <- cbind(dist,Inf)
path <- c()
h <- 900
while(length(queue)){
  t <- sample(seq_along(queue),1)
  cur <- queue[[t]]
  v <- cur$v
  current <- cur$current
  dir <- cur$dir
  queue <- queue[-t]
  directions <- 1:4
  lastdir <- tail(dir,1)
  posdir <- c(2:1,4:3)[!directions%in%lastdir]
  for(d in posdir){
    p <- nextp(current,d)
    r <- rle(c(dir,d))
    l <- tail(r$l,1)
    if(any(cur$path[,1]==p[1]&cur$path[,2]==p[2])) next
    if(l>10) next
    if(l<4 & d!=tail(dir,1)) next
    if(p[1]%in%c(0,nrow(map)+1)|p[2]%in%c(0,ncol(map)+1)) next
    newv <- v + map[p[1],p[2]]
    if(newv>h) next
    #cat(current,p,newv,"\n")
    ind <- dist[,1]==p[1]&
      dist[,2]==p[2]&
      dist[,3]==d&
      dist[,4]==l
    if(newv<dist[ind,5]){
      dist[ind,5] <- newv
      if(p[1]==nrow(map)&p[2]==ncol(map)){
        if(all(newv<dist[dist[,1]==13&dist[,2]==13,5])){
          path <- c(dir,d)
          h <- newv
          cat(h,"\n")
        }
      }else{
        queue[[length(queue)+1]] <- list(v=newv,current=p,dir=c(dir,d),path=rbind(cur$path,p))
      }
    }
  }
  cat(length(queue),"-",max(dist[is.finite(dist[,5]),1]),",",max(dist[is.finite(dist[,5]),2]),"-",min(dist[dist[,1]==nrow(map)&dist[,2]==nrow(map),5]),"\r")
}
cat("\n")
min(dist[dist[,1]==nrow(map)&dist[,2]==ncol(map),5])
write.table(dist,file="dist2.csv",sep="\t",row.names=FALSE)
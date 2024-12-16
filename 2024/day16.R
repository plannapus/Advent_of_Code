map <- read.map(16)
start <- which(map=="S",arr.ind=TRUE)
end <- which(map=="E",arr.ind=TRUE)
walls <- which(map=="#",arr.ind=TRUE)

dir <- "E"
dirs <- c("E","N","W","S")
inc <- matrix(c(0,1,-1,0,0,-1,1,0),ncol=2,byrow=TRUE)
maxx <- Inf

map2 <- map
map2[map2=="S"]<- 2
map2[map2=="E"]<- 2
map2[map2=="."]<- 1
map2[map2=="#"]<- 0
map2 <- apply(map2,2,as.integer)
par(mar=c(0,0,0,0))
image(1:141,1:141,map2,xlim=c(141,1))
queue <- list(list(x=start[1],y=start[2],pathd=dir,pathp=matrix(start,ncol=2),score=0))
memo <- cbind(which(map==".",arr.ind=TRUE),Inf)


while(length(queue)){
  l <- length(queue)
  pos <- queue[[l]]
  points(pos$x,pos$y,col="black",cex=0.5,pch=19)
  d <- substr(pos$pathd,nchar(pos$pathd),nchar(pos$pathd))
  nd <- switch(d,"E"="W","S"="N","W"="E","N"="S") #Direction to ignore
  p <- inc[dirs!=nd,]
  dd <- dirs[dirs!=nd]
  for(i in rev(seq_along(dd))){
    x <- pos$x+p[i,1]
    y <- pos$y+p[i,2]
    if(any(pos$pathp[,1]==x&pos$pathp[,2]==y)) next
    v <- map[x,y]
    if(v=="#") next
    if(v=="E"){
      if(dd[i]==d){ #if straight
        s <- pos$score+1
      }else{
        s <- pos$score+1001
      }
      if(s<maxx){
        maxx <- s
        bestpath <- list(pathd=paste(pos$pathd,dd[i],sep=""),
                         pathp=rbind(pos$pathp,c(x,y)))
        cat(maxx,"\n")
      }
    }else if(v!="#"){
      if(dd[i]==d){
        s <- pos$score+1
      }else{
        s <- pos$score+1001
      }
      path <- paste(pos$pathd,dd[i],sep="")
      S <- memo[memo[,1]==x&memo[,2]==y,3]
      if(s>S){
        next
      }else{
        memo[memo[,1]==x&memo[,2]==y,3] <- s
      }
      if(s<maxx){
        queue[[length(queue)+1]] <- list(x=x,
                                         y=y,
                                         pathd=path,
                                         pathp=rbind(pos$pathp,c(x,y)),
                                         score=s)
      }
    }
  }
  queue <- queue[-l]
  #cat(length(queue),":",maxx,"\r")
}
points(bestpath$pathp[,1],bestpath$pathp[,2],col="red",pch=19,cex=0.6)
#72428

dir <- "E"
dirs <- c("E","N","W","S")
inc <- matrix(c(0,1,-1,0,0,-1,1,0),ncol=2,byrow=TRUE)
maxx <- 100000

map2 <- map
map2[map2=="S"]<- 2
map2[map2=="E"]<- 2
map2[map2=="."]<- 1
map2[map2=="#"]<- 0
map2 <- apply(map2,2,as.integer)
par(mar=c(0,0,0,0))
image(1:141,1:141,map2,xlim=c(141,1))
queue <- list(list(x=start[1],y=start[2],pathd=dir,pathp=matrix(start,ncol=2),score=0))
w <- which(map==".",arr.ind=TRUE)
memo <- data.frame(x=rep(w[,1],each=4),y=rep(w[,2],each=4),d=c("E","N","S","W"),s=Inf)
bestpaths <- list()

while(length(queue)){
  l <- length(queue)
  pos <- queue[[l]]
  #points(pos$x,pos$y,col="black",cex=0.5,pch=19)
  d <- substr(pos$pathd,nchar(pos$pathd),nchar(pos$pathd))
  nd <- switch(d,"E"="W","S"="N","W"="E","N"="S") #Direction to ignore
  p <- inc[dirs!=nd,]
  dd <- dirs[dirs!=nd]
  for(i in rev(seq_along(dd))){
    x <- pos$x+p[i,1]
    y <- pos$y+p[i,2]
    if(any(pos$pathp[,1]==x&pos$pathp[,2]==y)) next
    v <- map[x,y]
    if(v=="#") next
    if(v=="E"){
      if(dd[i]==d){ #if straight
        s <- pos$score+1
      }else{
        s <- pos$score+1001
      }
      if(s<=maxx){
        maxx <- s
        bestpaths[[length(bestpaths)+1]] <- list(pathd=paste(pos$pathd,dd[i],sep=""),
                                                 pathp=rbind(pos$pathp,c(x,y)),score=s)
        cat(maxx,"\n")
      }
    }else if(v!="#"){
      if(dd[i]==d){
        s <- pos$score+1
      }else{
        s <- pos$score+1001
      }
      path <- paste(pos$pathd,dd[i],sep="")
      S <- memo[memo[,1]==x&memo[,2]==y&memo[,3]==dd[i],4]
      if(s>S){
        next
      }else{
        memo[memo[,1]==x&memo[,2]==y&memo[,3]==dd[i],4] <- s
      }
      if(s<=maxx){
        queue[[length(queue)+1]] <- list(x=x,
                                         y=y,
                                         pathd=path,
                                         pathp=rbind(pos$pathp,c(x,y)),
                                         score=s)
      }
    }
  }
  queue <- queue[-l]
  #cat(length(queue),":",maxx,"\r")
}

sc <- sapply(bestpaths,\(x)x$score)
bp <- bestpaths[sc==maxx]

for(i in seq_along(bp)){
  points(bp[[i]]$pathp[,1],bp[[i]]$pathp[,2],col="red",pch=19,cex=0.5)
}
pts <- do.call(rbind,lapply(bp,\(x)x$pathp))
nrow(pts[!duplicated(pts),])
#456

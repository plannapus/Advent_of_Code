input <- readLines("input14.txt")
input <- lapply(strsplit(input," -> "),
       function(x)apply(do.call(rbind,strsplit(x,",")),2,as.integer))
min(sapply(input,function(x)min(x[,2])))
map <- matrix(0,ncol=600,nrow=200)
for(i in seq_along(input)){
  for(j in 2:nrow(input[[i]])){
    map[input[[i]][j-1,2]:input[[i]][j,2],
        input[[i]][j-1,1]:input[[i]][j,1]]<-1
  }
}
s <- 1
while(TRUE){
  x <- 0
  y <- 500
  repeat{
    if(x>=nrow(map)) stop(s-1)
    if(map[x+1,y]==0){
      x <- x+1
    }else if(map[x+1,y-1]==0){
      x <- x+1
      y <- y-1
    }else if(map[x+1,y+1]==0){
      x <- x+1
      y <- y+1
    }else{
      map[x,y]<-2
      break
    }
  }
  cat(s,"\r")
  s <- s+1
}
#1133

map <- matrix(0,ncol=800,nrow=200)
for(i in seq_along(input)){
  for(j in 2:nrow(input[[i]])){
    map[input[[i]][j-1,2]:input[[i]][j,2],
        input[[i]][j-1,1]:input[[i]][j,1]]<-1
  }
}
w <- which(map==1,arr.ind=TRUE)
map[max(w[,1])+2,]<-1
s <- 1
while(TRUE){
  x <- 0
  y <- 500
  repeat{
    if(map[x+1,y]==0){
      x <- x+1
    }else if(map[x+1,y-1]==0){
      x <- x+1
      y <- y-1
    }else if(map[x+1,y+1]==0){
      x <- x+1
      y <- y+1
    }else{
      if(x==0) stop(s)
      map[x,y]<-2
      break
    }
  }
  cat(s,"\r")
  s <- s+1
}
map <- do.call(rbind,strsplit(readLines(read.input(6)),""))
guard <- which(map=="^",arr.ind=TRUE)
dir <- d <- "up"
steps <- matrix(guard,ncol=2)
repeat{
  if(dir=="up") new <- guard + c(-1,0)
  if(dir=="down") new <- guard + c(1,0)
  if(dir=="left") new <- guard + c(0,-1)
  if(dir=="right") new <- guard + c(0,1)
  if(new[1]<1 | new[1]>nrow(map) | new[2]<1 | new[2]>ncol(map)) break
  if(map[new[1],new[2]]=="#"){
    dir <- switch(dir,"up"="right","right"="down","down"="left","left"="up")
  }else{
    d <- c(d,dir)
    guard <- new
    steps <- rbind(steps,new)
  }
}
nrow(steps[!duplicated(steps),])
#5239


check_loop <- \(guard,dir,map){
  steps <- matrix(guard,ncol=2)
  d <- dir
  repeat{
    if(dir=="up") new <- guard + c(-1,0)
    if(dir=="down") new <- guard + c(1,0)
    if(dir=="left") new <- guard + c(0,-1)
    if(dir=="right") new <- guard + c(0,1)
    if(new[1]<1 | new[1]>nrow(map) | new[2]<1 | new[2]>ncol(map)) return(FALSE) #No Loop!
    if(map[new[1],new[2]]=="#"){
      dir <- switch(dir,"up"="right","right"="down","down"="left","left"="up")
    }else{
      guard <- new
      if(any(steps[,1]==guard[1] & steps[,2]==guard[2] & d==dir)) return(TRUE) #Loop!
      d <- c(d,dir)
      steps <- rbind(steps,new)
    }
  }
}

ob <- c()
d <- d[!duplicated(steps)] #Works because it is the second duplicate that is taken off
s <- steps[!duplicated(steps),]
for(i in 2:nrow(s)){ #quite slow
    if(map[s[i,1],s[i,2]]!="#"){
      mmap <- map
      mmap[s[i,1],s[i,2]] <- "#" #Place an obstacle
      guard <- s[i-1,] #Take position before that encountering that obstacle
      dir <- d[i-1]
      ob[i] <- check_loop(guard,dir,mmap)
    }
  cat(i,"\r")
}
sum(ob,na.rm=TRUE) #the first one is NA as it is the starting position.
#1753
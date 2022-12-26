input <- readLines("input24.txt")
map <- do.call(rbind,strsplit(input,""))

map_thru_t <- list(map)

for(t in 2:1001){
  m <- map_thru_t[[t-1]]
  M <- m
  M[] <- "."
  M[,c(1,152)]<-"#"
  M[c(1,22),]<-"#"
  M[1,2] <- M[22,151] <- "."
  for(i in 2:151){
    for(j in 2:21){
      left <- ifelse(i==2,m[j,151],m[j,i-1])
      if(grepl(">",left)) M[j,i] <- paste0(M[j,i],">")
      right <- ifelse(i==151,m[j,2],m[j,i+1])
      if(grepl("<",right)) M[j,i] <- paste0(M[j,i],"<")
      up <- ifelse(j==2,m[21,i],m[j-1,i])
      if(grepl("v",up)) M[j,i] <- paste0(M[j,i],"v")
      down <- ifelse(j==21,m[2,i],m[j+1,i])
      if(grepl("\\^",down)) M[j,i] <- paste0(M[j,i],"^")
      M[j,i] <- ifelse(nchar(M[j,i])>1,
                       gsub("^\\.","",M[j,i]),
                       M[j,i])
    }
  }
  map_thru_t[[t]] <- M
  cat(t,"\r")
}

best <- 1000
queue <- matrix(c(2,2,2,1),ncol=4,nrow=1)
while(nrow(queue)){
  queue <- queue[queue[,4]<=best,,drop=FALSE]
  cur <- queue[nrow(queue),]
  queue <- queue[-nrow(queue),,drop=FALSE]
  x <- cur[1]
  y <- cur[2]
  t <- cur[3]
  l <- cur[4]
  map <- map_thru_t[[t+1]]
  if(map[x-1,y]=="."&x!=2) queue <- rbind(queue,c(x-1,y,t+1,l+1))
  if(map[x+1,y]==".") queue <- rbind(queue,c(x+1,y,t+1,l+1))
  if(map[x,y-1]==".") queue <- rbind(queue,c(x,y-1,t+1,l+1))
  if(map[x,y+1]==".") queue <- rbind(queue,c(x,y+1,t+1,l+1))
  if(any(queue[,1]==22)){
    arrived <- queue[queue[,1]==22,,drop=FALSE]
    best <- min(c(best,arrived[,4]))
  }
  #queue <- queue[!duplicated(queue),,drop=FALSE]
  cat(nrow(queue),"-",min(queue[,3]),"-",max(queue[,3]),"-",best,"\r")
}

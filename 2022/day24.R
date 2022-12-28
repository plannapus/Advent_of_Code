input <- readLines("input24.txt")
map <- do.call(rbind,strsplit(input,""))

map_thru_t <- list(map)

for(t in 2:301){
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

step <- matrix(c(1,2),ncol=2)
t <- 0
repeat{
  t <- t+1
  targets <- matrix(nrow=0,ncol=2)
  for(i in 1:nrow(step)){
    dest <- rbind(step,
                  c(step[i,1]+1,step[i,2]),
                  c(step[i,1]-1,step[i,2]),
                  c(step[i,1],step[i,2]+1),
                  c(step[i,1],step[i,2]-1))
    dest <- dest[!(dest[,1]%in%c(0,23))&!(dest[,2]%in%c(0,153)),,drop=FALSE]
    safe <- apply(dest,1,\(x)map_thru_t[[(t%%300)+1]][x[1],x[2]]==".")
    dest <- dest[safe,,drop=FALSE]
    if(any(dest[,1]==22)) stop(t)
    targets <- rbind(targets,dest)
    targets <- targets[!duplicated(targets),]
  }
  step <- targets
  cat(t,"\r")
}
#322
step <- matrix(c(22,151),ncol=2)
repeat{
  t <- t+1
  targets <- matrix(nrow=0,ncol=2)
  for(i in 1:nrow(step)){
    dest <- rbind(step,
                  c(step[i,1]+1,step[i,2]),
                  c(step[i,1]-1,step[i,2]),
                  c(step[i,1],step[i,2]+1),
                  c(step[i,1],step[i,2]-1))
    dest <- dest[!(dest[,1]%in%c(0,23))&!(dest[,2]%in%c(0,153)),,drop=FALSE]
    safe <- apply(dest,1,\(x)map_thru_t[[(t%%300)+1]][x[1],x[2]]==".")
    dest <- dest[safe,,drop=FALSE]
    if(any(dest[,1]==1)) stop(t)
    targets <- rbind(targets,dest)
    targets <- targets[!duplicated(targets),]
  }
  step <- targets
  cat(t,"\r")
}
step <- matrix(c(1,2),ncol=2)
repeat{
  t <- t+1
  targets <- matrix(nrow=0,ncol=2)
  for(i in 1:nrow(step)){
    dest <- rbind(step,
                  c(step[i,1]+1,step[i,2]),
                  c(step[i,1]-1,step[i,2]),
                  c(step[i,1],step[i,2]+1),
                  c(step[i,1],step[i,2]-1))
    dest <- dest[!(dest[,1]%in%c(0,23))&!(dest[,2]%in%c(0,153)),,drop=FALSE]
    safe <- apply(dest,1,\(x)map_thru_t[[(t%%300)+1]][x[1],x[2]]==".")
    dest <- dest[safe,,drop=FALSE]
    if(any(dest[,1]==22)) stop(t)
    targets <- rbind(targets,dest)
    targets <- targets[!duplicated(targets),,drop=FALSE]
  }
  step <- targets
  cat(t,"\r")
}
#974
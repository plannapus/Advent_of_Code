input <- do.call(rbind,strsplit(readLines("input23.txt"),""))
pos <- which(input=="#",arr.ind=T)
dir <- rbind(c(-1,0),c(1,0),c(0,-1),c(0,1))
for(i in 1:10){
  new <- pos
  for(j in 1:nrow(pos)){
    k <- 1
    found <- FALSE
    while(!found){
      surround <- t(pos[j,]+t(expand.grid(-1:1,-1:1)))
      surround <- surround[!(surround[,1]==pos[j,1]&surround[,2]==pos[j,2]),]
      if(!any(apply(surround,1,function(x)any(pos[,1]==x[1]&pos[,2]==x[2])))){
        found <- TRUE
      }else{
        d <- dir[k,]
        if(d[1]==0){
          d <- cbind(-1:1,d[2])
        }else{
          d <- cbind(d[1],-1:1)
        }
        if(anyDuplicated(rbind(pos,t(pos[j,]+t(d))))){
          k <- k+1
        }else{
          new[j,] <- pos[j,]+dir[k,]
          found <- TRUE
        }
        if(k==5){
          found <- TRUE
        }
      }
    }
    if(!j%%10)cat(i,":",j,"\r")
  }
  if(anyDuplicated(new)){
    new[duplicated(new)|duplicated(new,fromLast=TRUE)] <- pos[duplicated(new)|duplicated(new,fromLast=TRUE)]
  }
  pos <- new
  dir <- dir[c(2:4,1),]
}
xr <- range(pos[,1])
yr <- range(pos[,2])
(diff(xr)+1)*(diff(yr)+1)-nrow(pos)
#4195

while(TRUE){
  i <- i+1
  new <- pos
  for(j in 1:nrow(pos)){
    k <- 1
    found <- FALSE
    while(!found){
      surround <- t(pos[j,]+t(expand.grid(-1:1,-1:1)))
      surround <- surround[!(surround[,1]==pos[j,1]&surround[,2]==pos[j,2]),]
      if(!any(apply(surround,1,function(x)any(pos[,1]==x[1]&pos[,2]==x[2])))){
        found <- TRUE
      }else{
        d <- dir[k,]
        if(d[1]==0){
          d <- cbind(-1:1,d[2])
        }else{
          d <- cbind(d[1],-1:1)
        }
        if(anyDuplicated(rbind(pos,t(pos[j,]+t(d))))){
          k <- k+1
        }else{
          new[j,] <- pos[j,]+dir[k,]
          found <- TRUE
        }
        if(k==5){
          found <- TRUE
        }
      }
    }
    if(!j%%10)cat(i,":",j,"\r")
  }
  if(anyDuplicated(new)){
    new[duplicated(new)|duplicated(new,fromLast=TRUE)] <- pos[duplicated(new)|duplicated(new,fromLast=TRUE)]
  }
  if(identical(pos,new)) stop(i)
  pos <- new
  dir <- dir[c(2:4,1),]
}

input <- read.table("input09.txt",sep=" ")
hpos <- tpos <- matrix(0,ncol=2)
for(i in 1:nrow(input)){
  d <- switch(input[i,1],"R"=c(1,0),
              "U"=c(0,1),"L"=c(-1,0),"D"=c(0,-1))
  for(j in 1:input[i,2]){
      hpos <- rbind(hpos,hpos[nrow(hpos),]+d)
  }
}

for(i in 1:nrow(hpos)){
  dif <- hpos[i,]-tail(tpos,1)
  if(any(abs(dif)>1)){
    if(dif[1]>=1&dif[2]==0) m <- c(1,0)
    if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
    if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
    if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
    if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
    if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
    if(dif[1]==0&dif[2]>=1) m <- c(0,1)
    if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
    tpos <- rbind(tpos, tail(tpos,1)+m)
  }
}
single <- tpos[!duplicated(tpos),]
nrow(single)
#6357

#Now tpos is position of knot 2
for(k in 3:10){
  hpos <- tpos
  tpos <- matrix(0,ncol=2)
  for(i in 1:nrow(hpos)){
    dif <- hpos[i,]-tail(tpos,1)
    if(any(abs(dif)>1)){
      if(dif[1]>=1&dif[2]==0) m <- c(1,0)
      if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
      if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
      if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
      if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
      if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
      if(dif[1]==0&dif[2]>=1) m <- c(0,1)
      if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
      tpos <- rbind(tpos, tail(tpos,1)+m)
    }
  }
}

single <- tpos[!duplicated(tpos),]
nrow(single)
#2627
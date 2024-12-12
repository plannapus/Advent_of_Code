input <- readLines(read.input(12))
# input2 <- readLines(textConnection("RRRRIICCFF
# RRRRIICCCF
# VVRRRCCFFF
# VVRCCCJFFF
# VVVVCJJCFE
# VVIVCCJJEE
# VVIIICJJEE
# MIIIIIJJEE
# MIIISIJEEE
# MMMISSJEEE"))
# map <- do.call(rbind,strsplit(input2,""))
map <- do.call(rbind,strsplit(input,""))
neigh <- \(x){
  ne <- rbind(c(0,1),c(1,0),c(-1,0),c(0,-1))
  a <- t(apply(ne,1,\(y)y+x))
  a[a[,1]>0&a[,2]>0&a[,1]<=nrow(map)&a[,2]<=ncol(map),,drop=FALSE]
}

find.area <- \(x){
  l <- map[x[1],x[2]]
  n <- neigh(x)
  area <- rbind(x,n[apply(n,1,\(y)map[y[1],y[2]]==l),,drop=FALSE])
  na <- 1
  j <- 2
  while(nrow(area)!=na){
    na <- nrow(area)
    for(i in j:nrow(area)){
      n <- neigh(area[i,])
      area <- rbind(area,n[apply(n,1,\(y)map[y[1],y[2]]==l),,drop=FALSE])
    }
    area <- area[!duplicated(area),]
    j <- na+1
  }
  area
}

perim <- \(a){
  ne <- rbind(c(0,1),c(1,0),c(-1,0),c(0,-1))
  nn <- matrix(ncol=2,nrow=0)
  p <- c()
  for(i in 1:nrow(a)){
    nn <- t(apply(ne,1,\(y)y+a[i,]))
    nn <- nn[!duplicated(nn),,drop=FALSE]
    nn <- nn[apply(nn,1,\(y)!any(y[1]==a[,1]&y[2]==a[,2])),,drop=FALSE]
    p[i] <- nrow(nn)
  }
  sum(p)
}


areas <- list()
i <- 1
j <- 1
k <- 1
ll <- c()
while(any(map!="")){
  if(map[i,j]!=""){
    ll[k] <- map[i,j]
    areas[[k]] <- find.area(c(i,j))
    for(m in 1:nrow(areas[[k]])){
      map[areas[[k]][m,1],areas[[k]][m,2]] <- ""
    }
    k <- k+1
  }
  i <- i+1
  if(i>nrow(map)){
    i <- 1
    j <- j+1
  }
  cat(i,j,k,"\r")
}

sum(sapply(areas,perim)*sapply(areas,nrow))
#1477924

sides <- \(a){
  if(nrow(a)==1) return(4)
  dx <- range(a[,1])
  dy <- range(a[,2])
  side <- 0
  at1 <- at2 <- matrix(ncol=2,nrow=0)
  for(i in dx[1]:dx[2]){
    j <- 0
    enc <- FALSE
    while(j<142){
      if(any(a[,1]==i&a[,2]==j)&!enc){
        enc <- TRUE
        at1 <- rbind(at1,c(i,j))
      }
      if(!any(a[,1]==i&a[,2]==j)&enc){
        enc <- FALSE
        at2 <- rbind(at2,c(i,j))
      }
      j <- j+1
    }
  }
  if(nrow(at1)>1){
    at1 <- at1[order(at1[,2],at1[,1]),]
    side <- side + sum(abs(diff(at1[,1]))+abs(diff(at1[,2]))!=1)+1
  }else{
    side <- side+1
  }
  if(nrow(at2)>1){
    at2 <- at2[order(at2[,2],at2[,1]),]
    side <- side + sum(abs(diff(at2[,1]))+abs(diff(at2[,2]))!=1)+1
  }else{
    side <- side+1
  }
  at1 <- at2 <- matrix(ncol=2,nrow=0)
  for(j in dy[1]:dy[2]){
    i <- 0
    enc <- FALSE
    while(i<142){
      if(any(a[,1]==i&a[,2]==j)&!enc){
        enc <- TRUE
        at1 <- rbind(at1,c(i,j))
      }
      if(!any(a[,1]==i&a[,2]==j)&enc){
        enc <- FALSE
        at2 <- rbind(at2,c(i,j))
      }
      i <- i+1
    }
  }
  if(nrow(at1)>1){
    at1 <- at1[order(at1[,1],at1[,2]),]
    side <- side + sum(abs(diff(at1[,1]))+abs(diff(at1[,2]))!=1)+1
  }else{
    side <- side+1
  }
  if(nrow(at2)>1){
    at2 <- at2[order(at2[,1],at2[,2]),]
    side <- side + sum(abs(diff(at2[,1]))+abs(diff(at2[,2]))!=1)+1
  }else{
    side <- side+1
  }
  return(side)
}

sum(sapply(areas,sides)*sapply(areas,nrow))
#841934
#Day 24 Puzzle 1
options(digits=22)
map = do.call(rbind,strsplit(readLines("input24.txt"),""))
new_state = function(map){
  eg = expand.grid(seq_len(nrow(map)),seq_len(ncol(map)))
  new_map = map
  neigh = matrix(c(0,1,0,-1,1,0,-1,0),ncol=2,byrow=TRUE)
  for(i in 1:nrow(eg)){
    content_n = table(factor(unlist(apply(neigh,1,function(x)if((x[1]+eg[i,1]>0 & x[2]+eg[i,2]>0)&(x[1]+eg[i,1]<=nrow(map) & x[2]+eg[i,2]<=ncol(map)))map[x[1]+eg[i,1],x[2]+eg[i,2]])),c(".","#")))
    content = map[eg[i,1],eg[i,2]]
    new_map[eg[i,1],eg[i,2]] = ifelse(content=="#" & content_n['#']!=1,'.', ifelse(content=="." & content_n['#']%in%1:2,"#",content))
  }
  new_map
}

id = function(map){paste(as.integer(map=="#"),collapse="")}

biodiv_rate = function(map){
  a = (seq_len(nrow(map)*ncol(map))-1)[t(map)=="#"]
  sum(2^a)
}
A=list(map)
n=1
repeat{
  map = new_state(map)
  n = n+1
  A[[n]]=map
  ids = sapply(A,id)
  if(any(duplicated(ids))) break
}
biodiv_rate(map)
#32776479

#Day 24 Puzzle 2 (#Redone in 2023)
map <- do.call(rbind,strsplit(readLines("input24.txt"),""))
#map <- do.call(rbind,strsplit(readLines("test24.txt"),""))
map[3,3] <- "?"
M <- list(list(level=0, map=map))
for(t in 1:200){
  levels <- sort(sapply(M,\(x)x$level))
  P <- list()
  p <- 0
  for(k in levels){
    lmap <- M[[which(sapply(M,\(x)x$level)==k)]]$map
    nmap <- lmap
    if((k-1)%in%levels) lower <- M[[which(sapply(M,\(x)x$level)==(k-1))]]$map
    if((k+1)%in%levels) upper <- M[[which(sapply(M,\(x)x$level)==(k+1))]]$map
    for(i in 1:5){
      for(j in 1:5){
        neigh <- c(if(i!=1) lmap[i-1,j],
                   if(i!=5) lmap[i+1,j],
                   if(j!=1) lmap[i,j-1],
                   if(j!=5) lmap[i,j+1])
        if((k+1)%in%levels){
          neigh <- c(neigh, if(i==1)upper[2,3], 
                     if(j==1)upper[3,2], 
                     if(j==5)upper[3,4], 
                     if(i==5)upper[4,3])
        }
        if((k-1)%in%levels){
          neigh <- c(neigh, 
                     if(i==2&j==3)lower[1,],
                     if(i==3&j==2)lower[,1],
                     if(i==4&j==3)lower[5,],
                     if(i==3&j==4)lower[,5]) 
        }
        if(lmap[i,j]=="#"){
          if(sum(neigh=="#")!=1) nmap[i,j] <- "."
        }
        if(lmap[i,j]=="."){
          if(sum(neigh=="#")%in%1:2) nmap[i,j] <- "#"
        }
        if(lmap[i,j]=="?" & !(k-1)%in%levels){
          lower <- matrix(".",nrow=5,ncol=5)
          lower[3,3] <- "?"
          if(lmap[i-1,j]=="#") lower[1,] <- "#"
          if(lmap[i+1,j]=="#") lower[5,] <- "#"
          if(lmap[i,j-1]=="#") lower[,1] <- "#"
          if(lmap[i,j+1]=="#") lower[,5] <- "#"
          if(any("#"%in%lower)){
            p <- p+1
            P[[p]] <- list(level=k-1,map=lower) 
          }
        }
      }
    }
    if(!(k+1)%in%levels){
      upper <- matrix(".",nrow=5,ncol=5)
      upper[3,3] <- "?"
      if(sum(lmap[1,]=="#")%in%1:2) upper[2,3] <- "#"
      if(sum(lmap[5,]=="#")%in%1:2) upper[4,3] <- "#"
      if(sum(lmap[,1]=="#")%in%1:2) upper[3,2] <- "#"
      if(sum(lmap[,5]=="#")%in%1:2) upper[3,4] <- "#"
      if(any("#"%in%upper)){
        p <- p+1
        P[[p]] <- list(level=k+1, map=upper)
      }
    }
    p <- p+1
    P[[p]] <- list(level=k, map=nmap)
  }
  M <- P
  cat(t,":",sum(sapply(M,\(x)sum(x$map=="#"))),"-",length(levels),"\r")
}

sum(sapply(M,\(x)sum(x$map=="#")))
#2017

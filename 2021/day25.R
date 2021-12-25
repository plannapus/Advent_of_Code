#input <- readLines("tests/test25.txt")
input <- readLines("input25.txt")
map <- do.call(rbind,strsplit(input,""))
step <- 0
repeat{
  step <- step+1
  map1 <- map
  map1[]<-"."
  map1[map=="v"]<-"v"
  east <- which(map==">",arr.ind=TRUE)
  east[,2] <- east[,2]+1
  east[east[,2]>ncol(map),2] <- 1
  for(i in 1:nrow(east)){
    if(map[east[i,1],east[i,2]]!="."){
      east[i,2]<-east[i,2]-1
      if(east[i,2]==0) east[i,2] <- ncol(map)
    }
    map1[east[i,1],east[i,2]]<-">"
  }
  map2 <- map1
  map2[] <- "."
  map2[map1==">"] <- ">"
  south <- which(map=="v",arr.ind=TRUE)
  south[,1] <- south[,1]+1
  south[south[,1]>nrow(map),1] <- 1
  for(i in 1:nrow(south)){
    if(map1[south[i,1],south[i,2]]!="."){
      south[i,1]<-south[i,1]-1
      if(south[i,1]==0) south[i,1] <- nrow(map)
    }
    map2[south[i,1],south[i,2]]<-"v"
  }
  if(all(map2==map)) break
  map <- map2
  cat(step,"\r")
}
step
#353
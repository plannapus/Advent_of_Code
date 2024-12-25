input <- readLines(read.input(25))
locks <- keys <- list()
w <- which(input=="")
for(i in seq_along(w)){
  a <- ifelse(i==1,1,w[i-1]+1)
  b <- w[i]-1
  map <- do.call(rbind,strsplit(input[a:b],""))
  map <- gsub("#","TRUE",map)
  map <- gsub("\\.","FALSE",map)
  map <- apply(map,2,as.logical)
  if(sum(map[1,])==0){
    keys[[length(keys)+1]] <- map
  }else{
    locks[[length(locks)+1]] <- map
  }
}


pairs <- 0
for(i in seq_along(keys)){
  for(j in seq_along(locks)){
    if(!any(keys[[i]]+locks[[j]]>1)){
      pairs <- pairs+1
    }
  }
  cat(i,"\r")
}
#3483
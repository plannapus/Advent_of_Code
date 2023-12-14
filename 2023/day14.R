input <- do.call(rbind,strsplit(readLines(read.input(14)),""))
#input <- do.call(rbind,strsplit(readLines("test/test14.txt"),""))
map <- input
for(i in 2:nrow(input)){
  for(j in 1:ncol(input)){
    if(input[i,j]=="O"){
      k <- i-1
      while(input[k,j]=="."){
        input[k+1,j] <- "."
        input[k,j] <- "O"
        k <- k-1
        if(k==0) break
      }
    }
  }
}
sum(rowSums(input=="O")*(100:1))
#107430

cycles <- 1000000000

cycle <- function(input){
  #North
  for(i in 2:nrow(input)){
    for(j in 1:ncol(input)){
      if(input[i,j]=="O"){
        k <- i-1
        while(input[k,j]=="."){
          input[k+1,j] <- "."
          input[k,j] <- "O"
          k <- k-1
          if(k==0) break
        }
      }
    }
  }
  #West
  for(i in 1:nrow(input)){
    for(j in 2:ncol(input)){
      if(input[i,j]=="O"){
        k <- j-1
        while(input[i,k]=="."){
          input[i,k+1] <- "."
          input[i,k] <- "O"
          k <- k-1
          if(k==0) break
        }
      }
    }
  }
  #South
  for(i in (nrow(input)-1):1){
    for(j in 1:ncol(input)){
      if(input[i,j]=="O"){
        k <- i+1
        while(input[k,j]=="."){
          input[k-1,j] <- "."
          input[k,j] <- "O"
          k <- k+1
          if(k>nrow(input)) break
        }
      }
    }
  }
  #East
  for(i in 1:nrow(input)){
    for(j in (ncol(input)-1):1){
      if(input[i,j]=="O"){
        k <- j+1
        while(input[i,k]=="."){
          input[i,k-1] <- "."
          input[i,k] <- "O"
          k <- k+1
          if(k>ncol(input)) break
        }
      }
    }
  }
  input
}
maps <- list()
maps[[1]] <- cycle(map)
for(i in 2:cycles){
  maps[[i]] <- cycle(maps[[i-1]])
  if(sum(maps[[i]]=="O")!=sum(map=="O")) break
  if(any(duplicated(maps))) break
  if(!i%%1000)cat(i,"\r")
}
l <- unique(which(duplicated(maps))-which(duplicated(maps,fromLast=TRUE)))
s <- which(duplicated(maps,fromLast=TRUE))
w <- sapply(maps,\(x)sum(rowSums(x=="O")*100:1))
w[(cycles-s)%%l+s]
#96317
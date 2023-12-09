input <- strsplit(readLines(read.input(9))," ")
input <- lapply(input,as.integer)
input <- input[1:200]
nv <- c()
for(i in seq_along(input)){
  d <- list(diff(input[[i]]))
  j <- 2
  while(any(d[[length(d)]])){
    d[[j]] <- diff(d[[j-1]])
    j <- j+1
  }
  for(j in length(d):1){
    if(j==length(d)){
      d[[j]] <- c(d[[j]],0)
    }else{
        d[[j]] <- c(d[[j]], d[[j]][length(d[[j]])]+d[[j+1]][length(d[[j+1]])])
    }
  }
  nv[i] <- input[[i]][length(input[[i]])]+d[[1]][length(d[[1]])]
}
sum(nv)
#1993300041

pv <- c()
for(i in seq_along(input)){
  d <- list(diff(input[[i]]))
  j <- 2
  while(any(d[[length(d)]])){
    d[[j]] <- diff(d[[j-1]])
    j <- j+1
  }
  for(j in length(d):1){
    if(j==length(d)){
      d[[j]] <- c(0,d[[j]])
    }else{
      d[[j]] <- c(d[[j]][1]-d[[j+1]][1],d[[j]])
    }
  }
  pv[i] <- input[[i]][1]-d[[1]][1]
}
sum(pv)
#1038
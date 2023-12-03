input <- readLines(read.input(3))
input <- do.call(rbind,strsplit(input,""))
N<-c()
places <- list()
k<-1
for(i in 1:nrow(input)){
  j <- 1
  while(j<=ncol(input)){
    if(input[i,j]%in%0:9){
      p <- paste(input[i,j:ncol(input)],collapse="")
      n <- regmatches(p,gregexpr("^[0-9]+",p))[[1]]
      m <- nchar(n)
      if(any(!input[pmax(i-1,0):pmin(i+1,nrow(input)),pmax((j-1),1):pmin(j+m,ncol(input))]%in%c(".",0:9))){
        N <- c(N,as.integer(n))
        places[[k]] <- cbind(i,j:(j+m-1))
        k <- k+1
      }
      j <- j+m
    }else{
      j <- j+1
    }
  }
}
sum(N)
#527369

gears <- which(input=="*",arr.ind=TRUE)
Q <- 0
for(g in 1:nrow(gears)){
  n <- 0
  P <- c()
  for(k in seq_along(places)){
    if(any(places[[k]][,1]%in%pmax(gears[g,1]-1,0):pmin(gears[g,1]+1,nrow(input))&
           places[[k]][,2]%in%pmax(gears[g,2]-1,0):pmin(gears[g,2]+1,ncol(input)))){
      n <- n+1
      P <- c(P,N[k])
    }
    cat(g,"/",k,"\r")
  }
  if(n>1) Q <- Q + prod(P)
}
Q
#73074886
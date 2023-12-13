input <- readLines(read.input(13))
w <- which(input=="")
maps <- list()
for(i in seq_along(w)){
  start <- ifelse(i==1,1,w[i-1]+1)
  end <- w[i]-1
  maps[[i]] <- input[start:end]
}
maps <- lapply(maps,\(x)do.call(rbind,strsplit(x,"")))

symx <- symy <- c()
for(i in seq_along(maps)){
  for(x in 1:(nrow(maps[[i]])-1)){
    X <- pmin(x,nrow(maps[[i]])-x)
    if(identical(maps[[i]][(x-X+1):x,], 
              maps[[i]][(x+X):(x+1),])){
      symx[i] <- x
      break
    }
  }
  for(y in 1:(ncol(maps[[i]])-1)){
    Y <- pmin(y,ncol(maps[[i]])-y)
    if(identical(maps[[i]][,(y-Y+1):y], 
                 maps[[i]][,(y+Y):(y+1)])){
      symy[i] <- y
      break
    }
  }
}
sum(symy,na.rm=TRUE)+100*sum(symx,na.rm=TRUE)
#34889

maps <- lapply(maps,\(x)apply(chartr(".#","01",x),2,as.integer))
symx2 <- symy2 <- c()
for(i in seq_along(maps)){
  for(x in 1:(nrow(maps[[i]])-1)){
    X <- pmin(x,nrow(maps[[i]])-x)
    if(sum(abs(maps[[i]][(x-X+1):x,]-maps[[i]][(x+X):(x+1),]))==1){
      symx2[i] <- x
      break
    }
  }
  for(y in 1:(ncol(maps[[i]])-1)){
    Y <- pmin(y,ncol(maps[[i]])-y)
    if(sum(abs(maps[[i]][,(y-Y+1):y]-maps[[i]][,(y+Y):(y+1)]))==1){
      symy2[i] <- y
      break
    }
  }
}

sum(symy2,na.rm=TRUE)+100*sum(symx2,na.rm=TRUE)
#34224
source("read.input.R")
options(digits=22)
input <- readLines(read.input(22))
input <- apply(do.call(rbind,strsplit(input,"[,~]")),2,as.integer)
input <- input[order(input[,3]),]
field <- array(0,dim=c(10,10,350))
for(i in 1:nrow(input)){
  field[(input[i,1]:input[i,4])+1,(input[i,2]:input[i,5])+1,(input[i,3]:input[i,6])+1]<-i
}
field2 <- field
for(i in 1:nrow(input)){
  cat(i,"\r")
  while(TRUE){
    w <- which(field==i,arr.ind=T)
    if(any(w[,3]==1)) break
    nw <- t(t(w)-c(0,0,1))
    a <- apply(nw,1,\(x)field[x[1],x[2],x[3]])
    if(all(a%in%c(0,i))){
      for(j in 1:nrow(w)){
        field[w[j,1],w[j,2],w[j,3]] <- 0
        field[nw[j,1],nw[j,2],nw[j,3]] <- i
      }
    }else{
      break
    }
  }
}
n <- rep(TRUE,nrow(input))
for(i in 1:nrow(input)){
  cat(i,"\r")
  field2 <- field
  w <- which(field==i,arr.ind=T)
  for(j in 1:nrow(w)) field2[w[j,1],w[j,2],w[j,3]] <- 0
  for(k in (1:nrow(input))[-i]){
    W <- which(field2==k,arr.ind=T)
    if(any(W[,3]==1)) next
    nw <- t(t(W)-c(0,0,1))
    a <- apply(nw,1,\(x)field2[x[1],x[2],x[3]])
    if(all(a%in%c(0,k))){
      n[i] <- FALSE
      break
    }
  }
}
sum(n)
#457

n <- m <- vector("list",length=nrow(input))
for(i in 1:nrow(input)){
  cat(i,"-",length(n),"\r")
  field2 <- field
  w <- which(field==i,arr.ind=T)
  for(j in 1:nrow(w)) field2[w[j,1],w[j,2],w[j,3]] <- 0
  repeat{
    s <- sum(field2==0)
    for(k in unique(field2[field!=0])){
      if(k%in%m[[i]]) next
      W <- which(field2==k,arr.ind=T)
      if(!length(W)) next
      if(any(W[,3]==1)) next
      nw <- t(t(W)-c(0,0,1))
      a <- apply(nw,1,\(x)field2[x[1],x[2],x[3]])
      if(all(a%in%c(0,k))){
        n[[i]] <- c(n[[i]],k)
        for(j in seq_len(nrow(W))) field2[W[j,1],W[j,2],W[j,3]] <- 0
        break
      }else{
        m[[i]]<-c(m[[i]],k)
      }
    }
    if(sum(field2==0)==s) break 
  }
}
sum(sapply(n,\(x)length(unique(x))))
#79122
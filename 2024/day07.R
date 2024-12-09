options(digits=22)
input <- lapply(strsplit(readLines(read.input(7)),"[: ]+"),as.numeric)
pos <- c()

check <- \(x,y){
  ap <- do.call(expand.grid,rep(list(c(1:2)),length(y)-1))
  for(i in 1:nrow(ap)){
    n <- y[1]
    for(j in 1:ncol(ap)){
      if(ap[i,j]==1) n <- n + y[j+1]
      if(ap[i,j]==2) n <- n * y[j+1]
    }
    if(abs(x-n)<1) return(round(n))
  }
  return(0)
}

m <- 0
M <- c()
ch <- c()
for(i in seq_along(head(input,-1))){
  ch[i] <- check(input[[i]][1],input[[i]][-1])
  M[i] <- ifelse(ch[i]!=0,TRUE,FALSE)
  m <- m + ch[i]
  if(is.na(m))break
  cat(i,":",m,"\r")
}
m
#1708857123053

f <- \(x,y,n){ #Recursivity for the win
  if(n>x) return(0)
  if(length(y)==0) if(abs(x-n)<1){return(x)}else{return(0)}
  n1 <- f(x,y[-1],n + head(y,1))
  n2 <- f(x,y[-1],n * head(y,1))
  n3 <- f(x,y[-1],as.numeric(paste(n,head(y,1),sep="")))
  if(any(c(n1,n2,n3)!=0)){
    return(x)
  }else{return(0)}
}

p <- 0
for(i in seq_along(head(input,-1))){
  if(M[i]){
    p <- p + ch[i]
  }else{
    p <- p + f(input[[i]][1],input[[i]][-1],0)
  } 
  cat(i,":",p,"\r")
}
p
#189207836795655


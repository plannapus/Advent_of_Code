source("read.input.R")
options(digits=22)
input <- readLines(read.input(12))
s <- strsplit(input," ")
onsen <- strsplit(sapply(s,\(x)x[1]),"")
arr <- lapply(strsplit(sapply(s,\(x)x[2]),","),as.integer)
res <- 0
check <- function(x,arr){
  r <- rle(x)
  identical(r$l[r$v=="#"],arr)
}
for(i in 1:1000){
  o <- onsen[[i]]
  s <- sum(o=="?")
  O <- matrix(o,nrow=2^s,ncol=length(o),byrow=T)
  S <- 2^((s:1)-1)
  q <- which(o=="?")
  for(j in seq_along(q)){
    O[,q[j]] <- rep(c(".","#"),each=S[j])
  }
  res <- res + sum(apply(O,1,check,arr[[i]]))
  cat(i,"\r")
}
res
#7195

check <- function(o,a){
  if(!any(o=="?")){ #full
    r <- rle(o)
    return(identical(r$l[r$v=="#"],a))
  }else{ #partial
    q <- which(o=="?")[1]
    r <- rle(o[1:(q-1)])
    R <- r$l[r$v=="#"]
    res <- TRUE
    if(length(R)>length(a)) return(FALSE)
    if(length(R)){
      if(o[q-1]=="."){
        res <- identical(R, a[1:length(R)])
      }else{
        res <- all(head(R,-1)==head(a,length(R)-1))& tail(R,1)<=a[length(R)]
      }
    }
    if(length(R)==length(a)){ 
      if(identical(R,a)){
        if("#"%in%o[q:length(o)]) res <- FALSE
      }
    }else{
      rest <- a[(length(R)+1):length(a)]
      res <- res & length(o[q:length(o)])>=(sum(rest)+length(rest)-1)
    }
    return(res)
  }
}


dfs <- function(o,a,i){
  #cat(o,"\t",i,"\n",file="test.txt",append=TRUE)
  if(i>length(o)){
    return(check(o,a))
  }else{
    r <- sum(o[1:i]=="#")
    c <- ifelse(o[i]=="?",i-1,i)
    if(r>sum(a)) return(0)
    if(r){
      if(!is.na(memo[c,r])) return(memo[c,r])
    }
    N <- 0
    if(o[i]%in%c(".","#")){
      if(check(o,a)){
        N <- N + dfs(o, a, i+1)
      }else{
        return(0)
      }
      r <- sum(o[1:i]=="#")
      memo[i,r] <<- N
    }
    if(o[i]=="?"){
      o1 <- o
      o1[i] <- "."
      if(check(o1,a)){
        N <- N + dfs(o1,a,i+1)
        r <- sum(o1[1:i]=="#")
        memo[i,r] <<- N
      }
      o2 <- o
      o2[i] <- "#"
      if(check(o2,a)){
        N <- N + dfs(o2,a,i+1)
        r <- sum(o1[1:i]=="#")
        memo[i,r] <<- N
      }
    }
    return(N) 
  }
}

input <- readLines(read.input(12))
#input <- readLines("test/test12.txt")
s <- strsplit(input," ")
onsen <- strsplit(sapply(s,\(x)paste(rep(x[1],5),collapse="?")),"")
arr <- lapply(strsplit(sapply(s,\(x)paste(rep(x[2],5),collapse=",")),","),as.integer)
res <- c()
for(i in 1:1000){
  #for(i in 1:6){
  cat(i,"\r")
  memo <- matrix(NA,nrow=length(onsen[[i]]),ncol=sum(arr[[i]]))
  res[i] <- dfs(onsen[[i]],arr[[i]],1)
}
sum(res)
# 33992866292225
#Day 1
## Part1
input <- readLines(read.input(2017,1))[1]
n <- 0
for(i in 1:nchar(input)){
  j <- ifelse(i==nchar(input),1,i+1)
  if(substr(input,i,i)==substr(input,j,j))n <- n+ as.integer(substr(input,i,i))
}
n
#1158
#Part2
n <- 0
for(i in 1:nchar(input)){
  j <- (i+nchar(input)/2)%%nchar(input)
  if(j==0) j <- nchar(input)
  if(substr(input,i,i)==substr(input,j,j))n <- n+ as.integer(substr(input,i,i))
}
n
#1132

## Day 2
# Part 1
input <- lapply(strsplit(readLines(read.input(2017,2)),"\t"),as.integer)
sum(sapply(input[1:16],\(x)abs(diff(range(x)))))
#34581

# Part 2
sum(sapply(input[1:16],\(x){
  cb <- combn(x,2)
  cb <- cbind(cb,cb[2:1,])
  a <- cb[,which(apply(cb,2,\(x)x[1]%%x[2])==0)]
  a[1]/a[2]
  }))
#214

## Day 3
# Part 1
input <- 361527
ceiling(sqrt(input))
#602
b <- 603^2 - input
d <- b - 602*3
602-d
#326

# Part 2
M <- matrix(ncol=603,nrow=603)
M[302,302] <- 1
for(i in seq(3,603,2)){
  n <- (max(M,na.rm=TRUE)+1):(i^2)
  w <- which(M==max(M,na.rm=TRUE),arr.ind=TRUE)
  a <- matrix(n,nrow=4,byrow=TRUE)
  M[w[1]:(w[1]-ncol(a)+1),w[2]+1] <- a[1,]
  M[(w[1]-ncol(a)+1),w[2]:(w[2]-ncol(a)+1)] <- a[2,]
  M[(w[1]-ncol(a)+1)+1:ncol(a),w[2]-ncol(a)+1] <- a[3,]
  M[(w[1]+1),(w[2]-ncol(a)+1)+1:ncol(a)] <- a[4,]
}
P <- matrix(ncol=603,nrow=603)
P[302,302] <- 1
i <- 2
repeat{
  w <- which(M==i,arr.ind=TRUE)
  P[w[1],w[2]] <- sum(P[w[1]+(-1:1),w[2]+(-1:1)],na.rm=TRUE)
  if(max(P,na.rm=TRUE)>input)stop(max(P,na.rm=TRUE))
  i <- i+1
  if(!i%%100)cat(i,":",max(P,na.rm=TRUE),"\r")
}
#363010

## Day 4
# Part 1
input <- strsplit(readLines(read.input(2017,4))," ")
sum(sapply(head(input,-1),anyDuplicated)==0)
#451

# Part 2
sum(sapply(head(input,-1),\(x)!any(duplicated(sapply(x,\(x)paste(sort(el(strsplit(x,""))),collapse=""))))))
#223

## Day 5
# Part 1
input <- scan(read.input(2017,5))
i <- 1
n <- 0
while(i>0 & i<= length(input)){
  j <- i
  i <- i+input[i]
  input[j] <- input[j] + 1
  n <- n+1
}
#388611

# Part 2
input <- scan(read.input(2017,5))
i <- 1
n <- 0
while(i>0 & i<= length(input)){
  j <- i
  i <- i+input[i]
  if(input[j]<3){
    input[j] <- input[j] + 1
  }else{
    input[j] <- input[j] - 1
  }
  n <- n+1
}
n
#27763113

#Day 6
## Part 1
input <- scan(read.input(2017,6))
m <- matrix(input,ncol=length(input))
repeat{
  w <- which.max(m[nrow(m),])
  hm <- m[nrow(m),w]
  new <- m[nrow(m),]
  new[w] <- 0
  for(i in 1:hm){
    w <- w+1
    if(w>ncol(m)) w <- 1
    new[w] <- new[w]+1
  }
  if(any(apply(m,1,\(x)all(new==x)))) stop(nrow(m))
  m <- rbind(m, new)
}
#6681

## Part 2
6681-which(apply(m,1,\(x)all(new==x)))+1
#2392

#Day 7
## Part 1
input <- readLines(read.input(2017,7))
p <- parse.group("^(?<a>.+) \\((?<w>[0-9]+)\\)( -> (?<b>.+))?",input)
s <- strsplit(p$b,", ")
names(s) <- p$a
edges <- matrix(ncol=2)
for(i in seq_along(s)){
  if(length(s[[i]])>0){
    edges <- rbind(edges,cbind(p$a[i],s[[i]]))
  }
}
library(igraph)
g <- graph_from_edgelist(edges[-1,],directed=TRUE)
plot(g,layout=layout_as_tree)
#dtacyn

## Part 2
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
p$w <- as.integer(p$w)
p$s <- 0
p$s[p$b==""] <- p$w[p$b==""]

repeat{
  for(i in seq_along(s)){
    if(length(s[[i]])>0){
      S <- p$s[p$a%in%s[[i]]]
      if(!any(is.na(S))){
        if(all(S==S[1])){
          p$s[i] <- sum(S)+p$w[i]
        }else{stop(i)}
      }
    }
  }
}
p[p$a%in%s[[i]]]
526-(1122-1117)

#521

# Day 8
input <- readLines(read.input(2017,8))
df <- parse.group("^(?<a>[a-z]+) (?<inst>[a-z]+) (?<n>[-0-9]+) if (?<b>[a-z]+) (?<eq>[<>=!]+) (?<m>[-0-9]+)$",head(input,-1))
u <- unique(c(df$a,df$b))
reg <- rep(0,length(u))
names(reg) <- u
for(i in 1:nrow(df)){
  b <- reg[df$b[i]]
  m <- as.integer(df$m[i])
  n <- as.integer(df$n[i])
  flag <- FALSE
  if(df$eq[i]=="!=") if(b!=m) flag <- TRUE
  if(df$eq[i]=="<") if(b<m) flag <- TRUE
  if(df$eq[i]=="<=") if(b<=m) flag <- TRUE
  if(df$eq[i]==">") if(b>m) flag <- TRUE
  if(df$eq[i]==">=") if(b>=m) flag <- TRUE
  if(df$eq[i]=="==") if(b==m) flag <- TRUE
  if(flag){
    if(df$inst[i]=="inc") reg[df$a[i]] <- reg[df$a[i]] + n
    if(df$inst[i]=="dec") reg[df$a[i]] <- reg[df$a[i]] - n
  }
}
max(reg)
# 5966

z <- max(reg)
reg <- rep(0,length(u))
names(reg) <- u
for(i in 1:nrow(df)){
  b <- reg[df$b[i]]
  m <- as.integer(df$m[i])
  n <- as.integer(df$n[i])
  flag <- FALSE
  if(df$eq[i]=="!=") if(b!=m) flag <- TRUE
  if(df$eq[i]=="<") if(b<m) flag <- TRUE
  if(df$eq[i]=="<=") if(b<=m) flag <- TRUE
  if(df$eq[i]==">") if(b>m) flag <- TRUE
  if(df$eq[i]==">=") if(b>=m) flag <- TRUE
  if(df$eq[i]=="==") if(b==m) flag <- TRUE
  if(flag){
    if(df$inst[i]=="inc") reg[df$a[i]] <- reg[df$a[i]] + n
    if(df$inst[i]=="dec") reg[df$a[i]] <- reg[df$a[i]] - n
    if(max(reg)>z) z <- max(reg)
  }
}
z
#6347

# Day 9
## Part 1
input <- scan(read.input(2017,9),"")
g <- rep(FALSE,nchar(input))
go <- rep(0,nchar(input))
gb <- FALSE
i <- 1
gbc <- 0
lvl <- 0
while(i<=nchar(input)){
  s <- substr(input,i,i)
  if(s=="{" & !gb){
    g[i] <- TRUE
    lvl <- lvl +1
    go[i] <- lvl
  }
  if(s=="}" & !gb){
    j <- tail(which(g),1)
    g[j] <- FALSE
    lvl <- lvl -1
  }
  if(s==">") gb <- FALSE
  if(s=="!"){
    i <- i+2
  }else{
    if(gb) gbc <- gbc+1
    i <- i+1
  }
  if(s=="<") gb <- TRUE
}
sum(go)
#16869

gbc
#87284

## Day 10
# Part 1
input <- scan(read.input(2017,10),sep=",")
ring <- 0:255
x <- 1
skip <- 0

for(i in seq_along(input)){
  y <- x+input[i]-1
  nn <- (x:y)%%256
  nn[nn==0] <- 256
  ring[nn] <- rev(ring[nn])
  x <- y+skip+1
  skip <- skip +1
}
prod(ring[1:2])
#7888

# Part 2
readLines(read.input(2017,10))->input
l <- c(as.integer(charToRaw(input[1])),17, 31, 73, 47, 23)
ring <- 0:255
x <- 1
skip <- 0

for(t in 1:64){
  for(i in seq_along(l)){
    y <- x+l[i]-1
    nn <- (x:y)%%256
    nn[nn==0] <- 256
    ring[nn] <- rev(ring[nn])
    x <- y+skip+1
    skip <- skip +1
  }
}

M <- matrix(ring,ncol=16,byrow=TRUE)
res <- rep(NA,16)
for(i in 1:16){
  a <- M[i,j]
  for(j in 2:16){
    a <- bitwXor(a,M[i,j])
  }
  res[i] <- a
}
paste(as.hexmode(res),collapse="")
# 2e9e81888d14ded85fa8228489ccdfaf
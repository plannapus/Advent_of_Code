source("read.input.R")
source("parse.group.R")
input <- readLines(read.input(24))
w <- which(input=="")[1]
reg <- parse.group("^(?<i>[a-z0-9]+): (?<b>[0-1])",input[1:(w-1)])
reg$b <- as.logical(as.integer(reg$b))

fromBitArr <- function(x) {
  x <- as.integer(x)
  y <- rev(x)
  num <- 0
  i <- 0
  for (c in y) {
    num <- num + c * (2^i)
    i <- i + 1
  }
  return(num)
}

gates <- parse.group("^(?<a>[a-z0-9]+) (?<inst>[A-Z]+) (?<b>[a-z0-9]+) -> (?<res>[a-z0-9]+)$",input[(w+1):(length(input)-1)])

ga <- gates
while(nrow(ga)){
  g <- rep(FALSE,nrow(ga))
  for(i in 1:nrow(ga)){
    if(ga$a[i]%in%reg$i&ga$b[i]%in%reg$i){
      ins <- ga$inst[i]
      a <- reg$b[reg$i==ga$a[i]]
      b <- reg$b[reg$i==ga$b[i]]
      if(length(a)>1 & length(b)>1) stop()
      if(ins=="AND") res <- a & b
      if(ins=="OR") res <- a | b
      if(ins=="XOR") res <- xor(a,b)
      reg <- rbind(reg,data.frame(i=ga$res[i], b=res))
      g[i] <- TRUE
    }
    cat(i,":",sum(g),"\r")
  }
  ga <- ga[!g,]
  cat(nrow(ga),sep="\r")
}

zs <- reg[grepl("^z",reg$i),]

n <- parse.group("z(?<n>[0-9]+)$",zs$i)
r <- zs$b[order(as.integer(n$n),decreasing=TRUE)]
options(digits=22)
fromBitArr(r)
#59336987801432

xs <- reg[grepl("^x",reg$i),]
nx <- parse.group("z(?<n>[0-9]+)$",xs$i)
rx <- xs$b[order(as.integer(nx$n),decreasing=TRUE)]

ys <- reg[grepl("^x",reg$i),]
ny <- parse.group("z(?<n>[0-9]+)$",ys$i)
ry <- ys$b[order(as.integer(ny$n),decreasing=TRUE)]

fromBitArr(rx&ry) #34106181008595

edges <- matrix(ncol=2,nrow=0)
for(i in 1:nrow(gates)){
  edges <- rbind(edges, c(gates$a[i],gates$res[i]),c(gates$b[i],gates$res[i]))
}
library(igraph)
g <- graph_from_edgelist(edges,directed=TRUE)


u <- unique(c(gates$a,gates$b,gates$res))
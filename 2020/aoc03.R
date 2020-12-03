###Part 1
input <- readLines("input03.txt")
map <- do.call(rbind,strsplit(input,""))
x <- 1
for(i in 2:nrow(map)){
  x[i] <- (x[i-1]+3)
  if(x[i]>ncol(map)) x[i] <- x[i] - ncol(map)
}

y <- 1:nrow(map)
sum(apply(cbind(x,y),1,function(X)map[X[2],X[1]])=="#")
#191

###Part 2
x1 <- x2 <- x3 <- x4 <- 1
for(i in 2:nrow(map)){
  x1[i] <- (x1[i-1]+1)
  if(x1[i]>ncol(map)) x1[i] <- x1[i] - ncol(map)
  x2[i] <- (x2[i-1]+3)
  if(x2[i]>ncol(map)) x2[i] <- x2[i] - ncol(map)
  x3[i] <- (x3[i-1]+5)
  if(x3[i]>ncol(map)) x3[i] <- x3[i] - ncol(map)
  x4[i] <- (x4[i-1]+7)
  if(x4[i]>ncol(map)) x4[i] <- x4[i] - ncol(map)
}

y1 <- 1:nrow(map)
y5 <- seq(1,nrow(map),2)

sum(apply(cbind(x1,y1),1,function(X)map[X[2],X[1]])=="#") * 
  sum(apply(cbind(x2,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x3,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x4,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x1[1:length(y5)],y5),1,function(X)map[X[2],X[1]])=="#") 
#1478615040


####Now the same but vectorized:
## Part 1
x <- ((1:nrow(map)-1)*3)%%ncol(map)+1
y <- 1:nrow(map)
sum(apply(cbind(x,y),1,function(X)map[X[2],X[1]])=="#")

## Part 2
x1 <- ((1:nrow(map)-1))%%ncol(map)+1
x2 <- ((1:nrow(map)-1)*3)%%ncol(map)+1
x3 <- ((1:nrow(map)-1)*5)%%ncol(map)+1
x4 <- ((1:nrow(map)-1)*7)%%ncol(map)+1
y1 <- 1:nrow(map)
y5 <- seq(1,nrow(map),2)

sum(apply(cbind(x1,y1),1,function(X)map[X[2],X[1]])=="#") * 
  sum(apply(cbind(x2,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x3,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x4,y1),1,function(X)map[X[2],X[1]])=="#") *
  sum(apply(cbind(x1[1:length(y5)],y5),1,function(X)map[X[2],X[1]])=="#") 

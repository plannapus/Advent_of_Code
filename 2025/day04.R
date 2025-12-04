input <- read.map(4)
map <- input
neighbours <- function(x){
  coords <- rbind(
    c(x[1]-1,x[2]),
    c(x[1]+1,x[2]),
    c(x[1],x[2]-1),
    c(x[1],x[2]+1),
    c(x[1]+1,x[2]+1),
    c(x[1]+1,x[2]-1),
    c(x[1]-1,x[2]+1),
    c(x[1]-1,x[2]-1))
  coords <- coords[coords[,1]>0&coords[,1]<137&
                     coords[,2]>0&coords[,2]<137,]
  sum(unlist(apply(coords,1,\(x)map[x[1],x[2]]=="@")))
}
rolls <- which(map=="@",arr.ind=TRUE)
nb <- apply(rolls,1,neighbours)
sum(nb<4)
#1395
M <- N <- sum(nb<4)
taken <- rolls[nb<4,]
rolls <- rolls[nb>=4,]
for(i in 1:nrow(taken)){
  map[taken[i,1],taken[i,2]] <- "x"
}
while(N>0){
  nb <- apply(rolls,1,neighbours)
  N <- sum(nb<4)
  M <- M+N
  if(N>0){
    taken <- rolls[nb<4,,drop=FALSE]
    rolls <- rolls[nb>=4,]
    for(i in 1:nrow(taken)){
      map[taken[i,1],taken[i,2]] <- "x"
    }
  }
  cat(M,"\n")
}
#8451
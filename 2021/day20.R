input <- readLines("input20.txt")
#input <- readLines("tests/test20.txt")
input <- chartr(".#","01",input)
alg <- as.integer(el(strsplit(input[1],"")))
st <- strsplit(input[-(1:2)],"")
map <- do.call(rbind,lapply(st,as.integer))
step <- function(map,alg){
  map2 <- rbind(rep(surrounding,ncol(map)+4),
               rep(surrounding,ncol(map)+4),
               cbind(rep(surrounding,nrow(map)),
                     rep(surrounding,nrow(map)),
                     map,
                     rep(surrounding,nrow(map)),
                     rep(surrounding,nrow(map))
                     ),
               rep(surrounding,ncol(map)+4),
               rep(surrounding,ncol(map)+4)
               )
  output <- map2
  output[] <- surrounding
  for(i in 2:(nrow(map2)-1)){
    for(j in 2:(ncol(map2)-1)){
      sect <- map2[(i-1):(i+1),(j-1):(j+1)]
      output[i,j] <- alg[sum(sect*2^matrix(8:0,byrow=T,nrow=3))+1]
    }
  }
  surrounding <<- ifelse(surrounding,tail(alg,1),head(alg,1))
  output[-c(1,nrow(output)),-c(1,ncol(output))]
}

surrounding <- 0
s1 <- step(map,alg)
res <- step(s1,alg)
sum(res)
#5097
surrounding <- 0
for(i in 1:50){
  map <- step(map,alg)
}
sum(map)
# 17987
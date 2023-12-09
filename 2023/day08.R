input <- readLines(read.input(8))
inst <- input[1]
map <- do.call(rbind,strsplit(input[3:length(input)],"[ =(,)]+"))
pos <- "AAA"
steps <- 0
i <- 1
while(pos!="ZZZ"){
  dir <- substr(inst,i,i)
  pos <- map[map[,1]==pos,ifelse(dir=="L",2,3)]
  steps <- steps +1
  i <- i+1
  if(i>nchar(inst)) i <- 1
}
#21409

pos <- map[grepl("A$",map[,1]),1]
res <- list()
for(j in seq_along(pos)){
  steps <- 0
  i <- 1
  r <- c()
  while(length(r)<4){
    dir <- substr(inst,i,i)
    pos[j] <- map[map[,1]==pos[j],ifelse(dir=="L",2,3)]
    steps <- steps +1
    i <- i+1
    if(i>nchar(inst)) i <- 1
    if(grepl("Z$",pos[j])) r <- c(r, steps)
  }
  res[[j]] <- r
}
options(digits=22)
res <- sapply(res,\(x)x[1])
all(!res%%nchar(inst))
for(i in 1:length(res)){
  if(i==1){
    lcm <- res[1]
  }else{
    q <- 2:pmin(lcm,res[i])
    lcm <- (lcm*res[i])/(q[!lcm%%q&!res[i]%%q])
  }
}
lcm
#21165830176709
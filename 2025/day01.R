input <- readLines(read.input(1))
input <- head(input,-1)
dir <- substr(input,1,1)
steps <- as.integer(substr(input,2,10))
cur <- 50
count <- 0
for(i in seq_along(dir)){
  cur <- cur + ifelse(dir[i]=="L", -1, 1)*steps[i]
  cur <- cur%%100
  if(cur==0) count <- count+1
}
count
#1191

cur <- 50
count <- 0
for(i in seq_along(dir)){
  u <- ifelse(dir[i]=="L", -1, 1)
  for(j in seq_len(steps[i])){
    cur <- cur + u
    cur <- cur%%100
    if(cur==0) count <- count+1
  }
}
count
#6858
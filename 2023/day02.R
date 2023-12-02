input <- el(strsplit(read.input(2),"\n"))

inp <- strsplit(input,"[:;]")
l <- lapply(inp,\(x)strsplit(x[-1],", ?"))
n <- 0
for(i in 1:100){
  L <- l[[i]]
  check <- TRUE
  for(j in seq_along(L)){
    p <- as.data.frame(parse.group("(?<n>[0-9]+) (?<c>[a-z]+)",L[[j]]))
    p[,1]<-as.integer(p[,1])
    if(!"red" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="red"))}
    if(!"green" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="green"))}
    if(!"blue" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="blue"))}
    if(p[p[,2]=="red",1]>12 | 
       p[p[,2]=="green",1]>13 | 
       p[p[,2]=="blue",1]>14) check <- FALSE
  }
  if(check) n <- n + i
}
n
#2169

n <- c()
for(i in 1:100){
  L <- l[[i]]
  red <- green <- blue <- 0
  for(j in seq_along(L)){
    p <- as.data.frame(parse.group("(?<n>[0-9]+) (?<c>[a-z]+)",L[[j]]))
    p[,1]<-as.integer(p[,1])
    if(!"red" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="red"))}
    if(!"green" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="green"))}
    if(!"blue" %in% p[,2]){p <- rbind(p, data.frame(n=0,c="blue"))}
    if(p$n[p$c=="red"]>=red) red <- p$n[p$c=="red"]
    if(p$n[p$c=="green"]>=green) green <- p$n[p$c=="green"]
    if(p$n[p$c=="blue"]>=blue) blue <- p$n[p$c=="blue"]
  }
  n[i] <- red*green*blue
}
sum(n)
#60948
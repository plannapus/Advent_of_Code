input <- scan(read.input(15),"",sep=",")
L <- lapply(input,\(x)as.integer(charToRaw(x)))
res <- c()
for(i in seq_along(L)){
  n <- 0
  for(j in seq_along(L[[i]])){
    n <- n+L[[i]][j]
    n <- n*17
    n <- n%%256
  }
  res[i] <- n
}
#509784
boxes <- vector("list",256)
for(i in 1:256) boxes[[i]] <- data.frame(l=c(),n=c())
p <- parse.group("^(?<b>[a-zA-Z]+)(?<o>[-=])(?<n>[0-9])*",input)
for(i in 1:nrow(p)){
  b <- as.integer(charToRaw(p$b[i]))
  N <- as.integer(p$n[i])
  n <- 0
  for(j in seq_along(b)){
    n <- n+b[j]
    n <- n*17
    n <- n%%256
  }
  if(p$o[i]=="="){
    if(!p$b[i]%in%boxes[[n+1]]$l) boxes[[n+1]] <- rbind(boxes[[n+1]], data.frame(l=p$b[i], n=N))
    if(p$b[i]%in%boxes[[n+1]]$l) boxes[[n+1]][boxes[[n+1]]$l==p$b[i],2] <- N
  }
  if(p$o[i]=="-"){
    if(p$b[i]%in%boxes[[n+1]]$l) boxes[[n+1]] <- boxes[[n+1]][boxes[[n+1]]$l!=p$b[i],]
  }
}
res <- 0
for(i in seq_along(boxes)){
  res <- res + sum(i*boxes[[i]]$n*seq_along(boxes[[i]]$n))
}
res
#230197
input <- readLines(read.input(5))
w <- which(input=="")
ord <- do.call(rbind,strsplit(input[1:(w[1]-1)],"\\|"))
lis <- strsplit(input[(w[1]+1):(w[2]-1)],",")
check <- \(x,l){
  pre <- ord[ord[,1]==x,]
  post <- ord[ord[,2]==x,]
  for(i in 1:nrow(pre)){
    if(pre[i,2]%in%l) if(which(l==pre[i,2])<which(l==x)) return(FALSE)
  }
  for(i in 1:nrow(post)){
    if(post[i,1]%in%l) if(which(l==post[i,1])>which(l==x)) return(FALSE)
  }
  TRUE
}

g <- sapply(lis,\(x)all(sapply(x,\(y)check(y,x))))
good <- lis[g]
sum(sapply(good,\(x)as.integer(x[(length(x)+1)/2])))
#5639

bad <- lis[!g]
correct <- \(l){
  rules <- ord[ord[,1]%in%l&ord[,2]%in%l,]
  start <- l[l%in%rules[,1]&!l%in%rules[,2]]
  p <- start
  L <- length(l)
  while(length(p)!=L){
    l <- l[!l%in%p]
    for(i in seq_along(l)){
      pre <- rules[rules[,2]==l[i],,drop=FALSE]
      if(nrow(pre)==length(p)){
        p <- c(p,l[i])
        break
      }
    }
  }
  p
}
s <- lapply(bad,correct)
sum(sapply(s,\(x)as.integer(x[(length(x)+1)/2])))
#5273
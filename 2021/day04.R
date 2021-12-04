# Part 1
picks <- scan("input04.txt",sep=",",nlines=1)
r <- readLines("input04.txt")
w <- which(r=="")
cards <- list()
for(i in seq_along(w)){
  d <- do.call(rbind,strsplit(gsub("^ +","",r[w[i]+1:5])," +"))
  cards[[i]] <- apply(d,2,as.integer)
}

cards1 <- cards
for(i in picks){
  cards1 <- lapply(cards1,function(x){x[x==i]<-NA;x})
  won <- sapply(cards1,function(x)any(rowSums(is.na(x))==5) | any(colSums(is.na(x))==5))
  if(any(won)) break
}
sum(cards1[won][[1]],na.rm=T)*i
#63552

# Part 2
cards2 <- cards
winning_order <- c()
for(i in picks){
  cards2 <- lapply(cards2,function(x){x[x==i]<-NA;x})
  won <- sapply(cards2,function(x)any(rowSums(is.na(x))==5) | any(colSums(is.na(x))==5))
  w <- which(won)
  winning_order <- c(winning_order,w[!w%in%winning_order])
  if(all(won)) break
}
sum(cards2[tail(winning_order,1)][[1]],na.rm=T)*i
#9020
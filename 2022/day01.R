input <- readLines("input01.txt")
w <- c(0,which(input==""))
elves <- list()
for(i in seq_along(w)){
  z <- ifelse(i==length(w),length(input),w[i+1]-1)
  elves[[i]]<-as.integer(input[(w[i]+1):z])
}
calories <- sapply(elves,sum)
max(calories)
#69528

sum(tail(sort(calories),3))
#206152
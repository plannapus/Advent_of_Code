input <- readLines("input25.txt")
tr <- cbind(c(2,1,0,"-","="),c(5:1)-3)
options(digits=22)
tot <- sum(sapply(strsplit(input,""),function(x){
  sum(sapply(x,function(y)as.integer(tr[tr[,1]==y,2]))*5^((length(x):1)-1))
}))
tot

res <- rep("0",21)
slots <- 5^(20:0)
i=1
while(tot!=0){
  w <- which.min(abs(tot - slots[i]*2:-2))
  res[i]<-tr[w,1]
  tot <- tot-slots[i]*(2:-2)[w]
  i <- i+1
}
gsub("^0+","",paste(res,collapse=""))
#2-=2==00-0==2=022=10
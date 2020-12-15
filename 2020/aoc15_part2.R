input <- c(13,16,0,12,15,1)
last <- 0
current <- 7
maxn <- 30000000
age <- rep(NA,maxn)
age[1+input]<-seq_along(input)
while(current<=maxn){
  x <- age[1+last]
  age[1+last] <- current
  if(is.na(x)){
    last <- 0
  }else{
    last <- current-x
  }
  current <- current + 1
  if(!current%%1000) cat(current,"\r")
}
which(age%in%maxn)-1
#2424

# Part 1
input <- c(13,16,0,12,15,1)
while(length(input)!=2020){
  if(tail(input,1)%in%head(input,-1)){
    age <- length(input)-tail(which(head(input,-1)==tail(input,1)),1)
    input <- c(input,age)
  }else{
    input <- c(input,0)
  }
}
input[2020]
#319

# Part 2
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

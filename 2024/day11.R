input <- scan(read.input(11))
options(digits=22)
stones <- input
#stones <- c(125,17)
for(t in 1:25){
  i <- 1
  while(i<=length(stones)){
    if(stones[i]==0){
      stones[i] <- 1
      i <- i+1
    }else if(!nchar(stones[i])%%2){
      n <- nchar(stones[i])/2
      new <- as.integer(c(substr(stones[i],1,n),
                          substr(stones[i],n+1,nchar(stones[i]))))
      if(i==1){
        stones <- c(new,stones[-i])
      }else if(i==length(stones)){
        stones <- c(stones[-i], new)
      }else{
        stones <- c(stones[1:(i-1)],new,stones[(i+1):length(stones)])
      }
      i <- i+2
    }else{
      stones[i] <- stones[i]*2024
      i <- i+1
    }
  }
  cat(t,":",length(stones),"\n")
}
length(stones)
#185894

blink <- \(x){
  if(x==0){
    return(1)
  }else if(!nchar(x)%%2){
    n <- nchar(x)/2
    a <- as.integer(substr(x,1,n))
    b <- as.integer(substr(x,n+1,nchar(x)))
    return(c(a,b))
  }else{
    return(x*2024)
  }
}

stones <- input
memo <- cbind(stones,1)

memorize <- \(x,n){
  if(!x%in%memo2[,1]){
    memo2 <<- rbind(memo2,c(x,n))
  }else{
    memo2[memo2[,1]==x,2] <<- memo2[memo2[,1]==x,2] + n
  }
}

for(t in 1:75){
  memo2 <- matrix(ncol=2,nrow=0)
  for(i in memo[,1]){
    for(j in blink(i)){
      memorize(j,memo[memo[,1]==i,2])
    }
  }
  memo <- memo2
  cat(t,"\r")
}
sum(memo[,2])
#221632504974231
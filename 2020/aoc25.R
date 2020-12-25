input <- scan("input25.txt")
cardpk <- input[1]
doorpk <- input[2]
n_loops <- function(n,to){
  start <- 1
  i <- 0
  while(start!=to){
   i <- i+1
   start <- (start*n)%%20201227
  }
  i
}
cardloop <- n_loops(7,cardpk)
doorloop <- n_loops(7,doorpk)

transform <- function(n,n_loop){
  start <- 1
  for(i in 1:n_loop){
    start <- (start*n)%%20201227
  }
  start
}
ek1 <- transform(cardpk,doorloop)
ek2 <- transform(doorpk,cardloop)
ek1==ek2
#TRUE
ek1
#9177528
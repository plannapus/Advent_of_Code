# Part 1
input <- scan("input06.txt",sep=",")
for(i in 1:80){
  input <- input -1
  if(any(input<0)){
    input <- c(input, rep(8,sum(input<0)))
    input[input<0] <- 6
  }
}
length(input)

# Part 2 (unfinished)
library(bit64)
input <- scan("input06.txt",sep=",")
tab <- integer64(9)
tab[1:9] <- as.vector(table(factor(input,0:8)))
for(i in 1:256){
  tab2 <- tab
  tab2[1:8]<-tab[2:9]
  tab2[9]<-tab[1]
  tab2[7] <- tab[8]+tab[1]
  tab <- tab2
}
sum(tab)

## Part 1
library(bit64) #native R can not handle number larger than 2147483647
input <- scan("input09.txt",integer64())
i=26
while(input[i]%in%apply(combn(input[i-25:1],2),2,sum)){i <- i+1}
cat(input[i])
#1492208709

## Part 2
magic <- input[i]
k  <- 2
while(!any(apply(embed(input,k),1,sum)==magic)){k <- k+1}
res <- embed(input,k)
sum(range(res[which(apply(res,1,sum)==magic),]))
#238243506
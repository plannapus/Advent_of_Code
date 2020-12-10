#Part 1
input <- scan("input10.txt")
chain <- c(0,sort(input),max(input)+3)
prod(table(diff(chain)))
#3000

#Part 2
options(digits=22)
r <- rle(diff(chain))
7^sum(r$l==4&r$v==1)*4^sum(r$l==3&r$v==1)*2^sum(r$l==2&r$v==1)
#193434623148032
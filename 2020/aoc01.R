# Part 1
input <- scan("input01.txt")
li <- do.call(rbind,lapply(input,function(x)if(any((x+input)==2020))c(which(input==x),which((x+input)==2020))))
prod(input[li[1,]])
# 913824

# Part2
n <- seq_along(input)
for(i in n){
  for(j in n[-i]){
    all <- input[i]+input[j]+input
    k <- which(all==2020)
    if(length(k)){
      stop(input[i]*input[j]*input[k])
    }
  }
}
# 240889536

input <- scan("input01.txt")
sum(diff(input)>0)
#1557

sum(diff(rowSums(embed(input,3)))>0)
#1608

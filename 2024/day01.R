input <- read.input(1)
input <- do.call(rbind,strsplit(readLines(input)," +"))
a <- apply(input,2,as.integer)
sum(abs(sort(a[,1])-sort(a[,2])))
#1830467

sum(a[,1]*sapply(a[,1],\(x)sum(a[,2]==x)))
#26674158
input <- lapply(strsplit(readLines(read.input(2))," "),as.integer)
d <- lapply(input,diff)[1:1000]
sum(sapply(d,\(x)length(table(sign(x)))==1 & all(abs(x)<=3)))
#314

safe <- rep(FALSE,1000)
for(i in 1:1000){
  for(j in seq_along(input[[i]])){
    d <- diff(input[[i]][-j])
    if((all(d<0)|all(d>0)) & all(abs(d)<=3)){safe[i] <- TRUE}
  }
}
sum(safe)
#373
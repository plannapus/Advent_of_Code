options(digits=22)
input <- scan(read.input(2),"",sep=",")
rg <- apply(do.call(rbind,strsplit(input,"-")),2,as.numeric)
rgs <- apply(rg,1,\(x)x[1]:x[2])
sum(sapply(rgs,\(x){
  x <- x[!nchar(x)%%2]
  count <- 0
  if(length(x)){
    for(i in seq_along(x)){
      if(substr(x[i],1,nchar(x[i])/2)==substr(x[i],1+nchar(x[i])/2,nchar(x[i]))){count <- count+x[i]}
    }
  }
  count
}))
#12586854255

sum(sapply(rgs,\(x){
  count <- 0
  for(i in seq_along(x)){
    if(grepl("^([0-9]{1,})\\1+$",x[i])){count <- count+x[i]}
  }
  count
}))
#17298174201
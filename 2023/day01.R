input <- readLines("input01.txt")
#input <- readLines("test01.txt")
g <- gsub("[^0-9]","",input)
s <- sapply(g,\(x)paste0(substr(x,1,1),substr(x,nchar(x),nchar(x))))
sum(as.integer(s))
#54338
df <- c("one","two","three","four","five","six","seven","eight","nine")
f <- function(x){
  n <- c()
  for(i in 1:nchar(x)){
    if(substr(x,i,i)%in%1:9) n <- c(n,substr(x,i,i))
    if(substr(x,i,i+2)%in%df) n <- c(n,(1:9)[df==substr(x,i,i+2)])
    if(substr(x,i,i+3)%in%df) n <- c(n,(1:9)[df==substr(x,i,i+3)])
    if(substr(x,i,i+4)%in%df) n <- c(n,(1:9)[df==substr(x,i,i+4)])
  }
  n <- n[!is.na(n)]
  as.integer(paste0(n[c(1,length(n))],collapse=""))
}
sum(sapply(input,f))
#53389

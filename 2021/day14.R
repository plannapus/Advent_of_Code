start <- readLines("input14.txt",n=1)
formula <- do.call(rbind,strsplit(readLines("input14.txt")[-(1:2)]," -> "))
for(j in 1:10){
step <- substr(start,1,1)
for(i in 2:nchar(start)){
  if(substr(start,i-1,i)%in%formula[,1]){
    step <- c(step, formula[formula[,1]==substr(start,i-1,i),2],substr(start,i,i))
  }else{
    step <- c(step, substr(start,i,i))
  }
}
start <- paste(step,collapse="")
cat(start,sep="\n")
}
tab <- table(el(strsplit(start,"")))
max(tab)-min(tab)
#2975
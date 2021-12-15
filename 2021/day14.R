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

library(bit64)
start <- readLines("input14.txt",n=1)
formula <- do.call(rbind,strsplit(readLines("input14.txt")[-(1:2)]," -> "))
comp1 <- paste0(substr(formula[,1],1,1),formula[,2])
comp2 <- paste0(formula[,2],substr(formula[,1],2,2))
step1 <- apply(embed(el(strsplit(start,"")),2),1,function(x)paste0(x[2],x[1]))
levels <- unique(c(comp1,comp2,formula[,1],step1))
tab <- integer64(length(levels))
tab[1:length(levels)] <- as.vector(table(factor(step1,levels)))
for(i in 1:40){
  step2 <- integer64(length(levels))
  for(j in 1:nrow(formula)){
    x <- tab[levels==formula[j,1]]
    step2[levels==comp1[j]] <- step2[levels==comp1[j]] + x
    step2[levels==comp2[j]] <- step2[levels==comp2[j]] + x
  }
  tab <- step2
}
names(tab)<-levels
by_let <- integer64(26)
for(i in seq_along(LETTERS)){
  n <- apply(do.call(rbind,strsplit(levels,"")),1,function(x)sum(x==LETTERS[i]))
  x <- sum(tab*n)
  if(LETTERS[i]%in%c(substr(start,1,1),substr(start,nchar(start),nchar(start)))){x <- x+1} #If 1rst or last letter add 1
  by_let[i] <- x/2
}
max(by_let)-min(by_let[by_let!=0])
#3015383850689

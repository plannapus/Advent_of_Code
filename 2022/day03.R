rucksacks <- strsplit(readLines("input03.txt"),"")
priority <- function(x)which(c(letters,LETTERS)==x)
sum(sapply(rucksacks, function(x){
  M <- matrix(x,ncol=2,byrow=FALSE)
  priority(unique(M[M[,1]%in%M[,2],1]))
  }))
#8039

n <- rep(0,length(rucksacks))
for(i in seq(1,length(rucksacks),3)){
  u <- unique(rucksacks[[i]][rucksacks[[i]]%in%rucksacks[[i+1]]])
  n[i] <- priority(unique(u[u%in%rucksacks[[i+2]]]))
}
sum(n)
#2510
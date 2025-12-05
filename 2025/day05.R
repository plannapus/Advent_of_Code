options(digits=22)
input <- read.input(5)
input <- readLines(input)
w <- which(input=="")
fresh <- input[1:(w[1]-1)]
ing <- input[(w[1]+1):(w[2]-1)]
fresh <- do.call(rbind,strsplit(fresh,"-"))
fresh <- apply(fresh,2,as.numeric)
ing <- as.numeric(ing)
check <- sapply(ing,\(x){
  f <- fresh[fresh[,1]<=x&fresh[,2]>=x,,drop=FALSE]
  nrow(f)>0
  })
sum(check)
#811

#sum(apply(fresh,1,diff)+1) #Nope

fresh <- fresh[order(fresh[,1],fresh[,2]),]
simp <- fresh[1,,drop=FALSE]
for(i in 2:nrow(fresh)){
  if(fresh[i,1]<=simp[nrow(simp),2]){
    if(fresh[i,2]>simp[nrow(simp),2]){
      simp[nrow(simp),2] <- fresh[i,2]
    }else{
      next
    }
  }else{
    simp <- rbind(simp,fresh[i,])
  }
}
sum(apply(simp,1,diff)+1)
#338189277144473
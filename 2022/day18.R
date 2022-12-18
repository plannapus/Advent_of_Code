input <- read.table("input18.txt",sep=",")
d <- dist(input,"manhattan")
sum(6-rowSums(as.matrix(d)==1))
#4340

sides <- do.call(rbind,lapply(seq_len(nrow(input)),function(x){
  rbind(c(input[x,1]+1,input[x,2],input[x,3]),
        c(input[x,1]-1,input[x,2],input[x,3]),
        c(input[x,1],input[x,2]+1,input[x,3]),
        c(input[x,1],input[x,2]-1,input[x,3]),
        c(input[x,1],input[x,2],input[x,3]+1),
        c(input[x,1],input[x,2],input[x,3]-1))
}))
A <- apply(sides,1,function(x)anyDuplicated(rbind(x,input)))
sides <- sides[!A,]

ext <- c()
for(i in 1:nrow(sides)){
  ext[i]<-any(all(!(input[,1]%in%-1:sides[i,1]&input[,2]==sides[i,2]&input[,3]==sides[i,3])),
      all(!(input[,1]%in%sides[i,1]:22&input[,2]==sides[i,2]&input[,3]==sides[i,3])),
      all(!(input[,2]%in%-1:sides[i,2]&input[,1]==sides[i,1]&input[,3]==sides[i,3])),
      all(!(input[,2]%in%sides[i,2]:22&input[,1]==sides[i,1]&input[,3]==sides[i,3])),
      all(!(input[,3]%in%-1:sides[i,3]&input[,1]==sides[i,1]&input[,2]==sides[i,2])),
      all(!(input[,3]%in%sides[i,3]:22&input[,1]==sides[i,1]&input[,2]==sides[i,2])))
}
sum(ext)+5
#2468
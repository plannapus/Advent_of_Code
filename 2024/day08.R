input <- do.call(rbind,strsplit(readLines(read.input(8)),""))
#input <- do.call(rbind,strsplit(readLines("test08.txt"),""))
types <- unique(input[input!="."])
node <- list()
f <- \(u,v){
  d <- u-v
  list(u+d,v-d)
}
for(i in seq_along(types)){
  w <- which(input==types[i],arr.ind=TRUE)
  n <- nrow(w)
  cb <- combn(1:n,2)
  a<-apply(cb,2,\(x)f(w[x[1],],w[x[2],]))
  node[[i]] <- do.call(rbind,lapply(a,\(x)do.call(rbind,x)))
}
nexus <- do.call(rbind,node)
nexus <- nexus[!duplicated(nexus),]
nexus <- nexus[nexus[,1]>0 & nexus[,2]>0,]
nexus <- nexus[nexus[,1]<=nrow(input) & nexus[,2]<=ncol(input),]
nrow(nexus)
#361

node <- list()
f <- \(u,v){
  d <- u-v
  cbind(u[1]+d[1]*(-50:50),u[2]+d[2]*(-50:50))
}
for(i in seq_along(types)){
  w <- which(input==types[i],arr.ind=TRUE)
  n <- nrow(w)
  cb <- combn(1:n,2)
  nd <- c()
  for(j in 1:ncol(cb)){
    nd <- rbind(nd,f(w[cb[1,j],],w[cb[2,j],]))
  }
  node[[i]] <- nd
}
nexus <- do.call(rbind,node)
nexus <- nexus[!duplicated(nexus),]
nexus <- nexus[nexus[,1]>0 & nexus[,2]>0,]
nexus <- nexus[nexus[,1]<=nrow(input) & nexus[,2]<=ncol(input),]
nrow(nexus)
#1249
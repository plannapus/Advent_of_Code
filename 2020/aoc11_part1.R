# Part1
input <- readLines("input11.txt")
map <- do.call(rbind,strsplit(input,""))
repeat{
  neighbours <- array(dim=c(nrow(map),ncol(map),8))
  neighbours[,,1]<-rbind(".",map[-nrow(map),])
  neighbours[,,2]<-rbind(map[-1,],".")
  neighbours[,,3]<-cbind(".",map[,-ncol(map)])
  neighbours[,,4]<-cbind(map[,-1],".")
  neighbours[,,5]<-rbind(cbind(map[-1,-1],"."),".")
  neighbours[,,6]<-rbind(cbind(".",map[-1,-ncol(map)]),".")
  neighbours[,,7]<-rbind(".",cbind(".",map[-nrow(map),-ncol(map)]))
  neighbours[,,8]<-rbind(".",cbind(map[-nrow(map),-1],"."))
  n_n <- apply(neighbours,c(1,2),function(x)sum(x=="#"))
  step0 <- array(dim=c(nrow(map),ncol(map),2))
  step0[,,1]<-map
  step0[,,2]<-n_n
  step <- apply(step0,c(1,2),function(x)if(x[1]=="L"&x[2]==0){"#"}else if(x[1]=="#"&as.integer(x[2])>3){"L"}else{x[1]})
  if(identical(step,map)) break
  map <- step
}
cat("\n")
sum(step=="#")
#2483
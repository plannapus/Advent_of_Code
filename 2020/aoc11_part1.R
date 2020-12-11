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
sum(step=="#")
#2483

map <- do.call(rbind,strsplit(input,""))
count_seats <- function(X,map){
  x <- X[1]
  y <- X[2]
  n <- 0
  if(x!=1){
    n <- n + grepl("^\\.*#",paste(map[(x-1):1,y],collapse=""))
    if(y!=1){
      n <- n + grepl("^\\.*#",paste(diag(map[(x-1):1,(y-1):1,drop=FALSE]),collapse=""))
    }
    if(y!=ncol(map)){
      n <- n + grepl("^\\.*#",paste(diag(map[(x-1):1,(y+1):ncol(map),drop=FALSE]),collapse=""))
    }
  }
  if(x!=nrow(map)){
    n <- n + grepl("^\\.*#",paste(map[(x+1):nrow(map),y],collapse=""))
    if(y!=1){
      n <- n + grepl("^\\.*#",paste(diag(map[(x+1):nrow(map),(y-1):1,drop=FALSE]),collapse=""))
    }
    if(y!=ncol(map)){
      n <- n + grepl("^\\.*#",paste(diag(map[(x+1):nrow(map),(y+1):ncol(map),drop=FALSE]),collapse=""))
    }
  }
  if(y!=ncol(map)){
    n <- n + grepl("^\\.*#",paste(map[x,(y+1):ncol(map)],collapse=""))
  }
  if(y!=1){
    n <- n + grepl("^\\.*#",paste(map[x,(y-1):1],collapse=""))
  }
  n
}

m <- 0
repeat{
  m <- m+1
  step <- map
  taken <- which(map=="#",arr.ind=T)
  empty <- which(map=="L",arr.ind=T)
  if(!nrow(taken)){
    step[map=="L"]<-"#"
  }else{
    next_t<-apply(taken,1,count_seats,map)
    move <- taken[next_t>=5,]
    if(nrow(move)){
      for(i in 1:nrow(move)){
        step[move[i,1],move[i,2]] <- "L"
      }
    }
    if(nrow(empty)){
      next_e <- apply(empty,1,count_seats,map)
      move <- empty[next_e==0,]
      if(nrow(move)){
        for(i in 1:nrow(move)){
          step[move[i,1],move[i,2]] <- "#"
        }
      }
    }
  }
  if(identical(step,map)) break
  map <- step
  cat(m,"\n")
  write(t(map),ncolumns=ncol(map),file="",sep="")
}
cat("\n")
sum(step=="#")
#3146 WRONG

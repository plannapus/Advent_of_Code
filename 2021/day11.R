input <- readLines("input11.txt")
map <- apply(do.call(rbind,strsplit(input,"")),2,as.integer)
light <- 0
neighbours <- function(x,y){
  o <- expand.grid(row=x+(-1:1),col=y+(-1:1))
  o <- o[!(o[,1]==x&o[,2]==y),]
  o <- o[o[,1]>0 & o[,2]>0,]
  o <- o[o[,1]<=nrow(map) & o[,2]<=ncol(map),]
  o
}
n_steps <- 100
for(i in 1:n_steps){
  map2 <- map+1
  if(any(map2>9)){
    w <- which(map2>9,arr.ind=T)
    k <- 1
    while(k<=nrow(w)){
      n <- neighbours(w[k,1],w[k,2])
      for(j in 1:nrow(n)){
        map2[n[j,1],n[j,2]]<-map2[n[j,1],n[j,2]]+1
        if(map2[n[j,1],n[j,2]]>9){
          w <- rbind(w,n[j,])
          w <- w[!duplicated(w),]
        }
      }
      k <- k+1
    }
    light <- light + nrow(w)
    map2[map2>9]<-0
  }
  map <- map2
}
light
#1649

map <- apply(do.call(rbind,strsplit(input,"")),2,as.integer)
i <- 1
repeat{
  map2 <- map+1
  if(any(map2>9)){
    w <- which(map2>9,arr.ind=T)
    k <- 1
    while(k<=nrow(w)){
      n <- neighbours(w[k,1],w[k,2])
      for(j in 1:nrow(n)){
        map2[n[j,1],n[j,2]]<-map2[n[j,1],n[j,2]]+1
        if(map2[n[j,1],n[j,2]]>9){
          w <- rbind(w,n[j,])
          w <- w[!duplicated(w),]
        }
      }
      k <- k+1
    }
    map2[map2>9]<-0
  }
  map <- map2
  if(all(map==0)) break
  i <- i+1
  if(!i%%100)cat(i,"\r")
}
i
#256
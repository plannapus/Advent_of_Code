##Part1
input <- readLines("input17.txt")
map <- do.call(rbind,strsplit(input,""))
#map <- rbind(c(".","#","."),c(".",".","#"),c("#","#","#"))
library(reshape)
m <- melt(map)
m <- data.frame(x=m$X1,y=m$X2,z=0,value=m$value)
neighbours <- function(m){
  mask <- expand.grid(-1:1,-1:1,-1:1)
  M <- as.matrix(m[,1:3])
  l <- do.call(rbind,lapply(seq_len(nrow(m)),function(i)t(M[i,]+t(mask))))
  l <- l[!duplicated(l),]
  data.frame(x=l[,1],y=l[,2],z=l[,3])
}
#M<-list()
#M[[i]]<-m
for(i in 1:6){
  p <- neighbours(m)
  p$value <- NA
  coords <- apply(m[,1:3],1,paste,collapse=",")
  for(j in 1:nrow(p)){
    content <- table(sapply(apply(neighbours(p[j,]),1,paste,collapse=","),function(x)factor(ifelse(x%in%coords,m$value[coords==x],"."),levels=c(".","#"))))
    this_cube <- paste(p[j,1:3],collapse=",")
    here <- ifelse(this_cube%in%coords,m$value[coords==this_cube],".")
    if(here=="#"&content["#"]%in%3:4){
      p$value[j] <- "#"
    }else if(here=="."&content["#"]==3){
      p$value[j] <- "#"
    }else{
      p$value[j]<-"."
    }
  }
  m<-p
 # M[[i+1]]<-m
}
sum(m$value=="#")
#306

##Part2 (fairly slow, i. e. ca. 2h)
m <- melt(map)
m <- data.frame(x=m$X1,y=m$X2,z=0,w=0,value=m$value)
neighbours <- function(m){
  mask <- expand.grid(-1:1,-1:1,-1:1,-1:1)
  M <- as.matrix(m[,1:4])
  l <- do.call(rbind,lapply(seq_len(nrow(m)),function(i)t(M[i,]+t(mask))))
  l <- l[!duplicated(l),]
  data.frame(x=l[,1],y=l[,2],z=l[,3],w=l[,4])
}
for(i in 1:6){
  cat(i,":\n")
  p <- neighbours(m)
  p$value <- NA
  coords <- apply(m[,1:4],1,paste,collapse=",")
  for(j in 1:nrow(p)){
    cat(j,"\r")
    content <- table(sapply(apply(neighbours(p[j,]),1,paste,collapse=","),function(x)factor(ifelse(x%in%coords,m$value[coords==x],"."),levels=c(".","#"))))
    this_cube <- paste(p[j,1:4],collapse=",")
    here <- ifelse(this_cube%in%coords,m$value[coords==this_cube],".")
    if(here=="#"&content["#"]%in%3:4){
      p$value[j] <- "#"
    }else if(here=="."&content["#"]==3){
      p$value[j] <- "#"
    }else{
      p$value[j]<-"."
    }
  }
  m<-p
  cat("\n")
}
sum(m$value=="#")
#2572

##Part2 (faster but still slow, i. e. ~ 1h)
m <- melt(map)
m <- data.frame(x=m$X1,y=m$X2,z=0,w=0,value=m$value)
neighbours <- function(m){
  mask <- expand.grid(-1:1,-1:1,-1:1,-1:1)
  M <- as.matrix(m[,1:4])
  l <- do.call(rbind,lapply(seq_len(nrow(m)),function(i)t(M[i,]+t(mask))))
  l <- l[!duplicated(l),]
  data.frame(x=l[,1],y=l[,2],z=l[,3],w=l[,4])
}
compute_values<-function(p,m,coords){
  content <- table(sapply(apply(neighbours(p),1,paste,collapse=","),function(x)factor(ifelse(x%in%coords,m$value[coords==x],"."),levels=c(".","#"))))
  this_cube <- paste(p[1,1:4],collapse=",")
  here <- ifelse(this_cube%in%coords,m$value[coords==this_cube],".")
  ifelse((here=="#"&content["#"]%in%3:4)|(here=="."&content["#"]==3),"#",".")
}
for(i in 1:6){
  cat(i,"\n")
  p <- neighbours(m)
  coords <- apply(m[,1:4],1,paste,collapse=",")
  p$value <- sapply(seq_len(nrow(p)), function(x) compute_values(p[x,],m,coords))
  m <- p
}
cat(sum(m$value=="#"))

##Part1
input <- readLines("input24.txt")
#input <- readLines("test24.txt")
input <- gsub("([ew])","\\1 ",input)
input <- strsplit(input," ")
#Using this coordinate system: https://www.redblobgames.com/grids/hexagons/#coordinates-cube
dif <- as.data.frame(do.call(rbind,list(e = c(1,-1,0),
                   w = c(-1,1,0),
                   sw = c(-1,0,1),
                   nw = c(0,1,-1),
                   se = c(0,-1,1),
                   ne = c(1,0,-1))))
for(i in seq_along(input)){
  tab <- table(factor(input[[i]],c("e","w","sw","nw","se","ne")))
  tile <- colSums(tab*dif)
  if(i!=1){
    if(paste(tile,collapse="_")%in%apply(flipped,1,paste,collapse="_")){
      flipped <- flipped[apply(flipped,1,paste,collapse="_")!=paste(tile,collapse="_"),]
    }else{
      flipped <- rbind(flipped,tile)
    }
  }else{
    flipped <- matrix(tile,ncol=3)
  }
}
nrow(flipped)
#485

##Part2 (slow)
colnames(flipped)<-c("x","y","z")
tiles <- as.data.frame(flipped)
tiles$value <- 1
neighbours <- function(m){ #Returns a list of neighbours for a given point or a full region
  M <- as.matrix(m[,1:3])
  l <- do.call(rbind,lapply(seq_len(nrow(m)),function(i)t(M[i,]+t(dif))))
  l <- l[!duplicated(l),]
  data.frame(x=l[,1],y=l[,2],z=l[,3])
}
cat("Day 0: ",nrow(tiles),"\n",sep="")
for(i in 1:100){
  p <- neighbours(tiles)
  p$value <- 0
  coords <- apply(tiles[,1:3],1,paste,collapse=",")
  for(j in 1:nrow(p)){
    content <- table(sapply(apply(neighbours(p[j,]),1,paste,collapse=","),function(x)factor(ifelse(x%in%coords,tiles$value[coords==x],"."),levels=c(0,1))))
    this_cube <- paste(p[j,1:3],collapse=",")
    here <- ifelse(this_cube%in%coords,tiles$value[coords==this_cube],0)
    if(here==1&content["1"]%in%1:2){
      p$value[j] <- 1
    }else if(here==0&content["1"]==2){
      p$value[j] <- 1
    }
  }
  tiles <- p[p$value==1,]
  cat("Day ",i,": ",nrow(tiles),"\n",sep="\n")
}
sum(tiles$value)
#
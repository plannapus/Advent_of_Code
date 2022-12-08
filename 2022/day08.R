input <- apply(do.call(rbind,strsplit(readLines("input08.txt"),"")),2,as.integer)
n <- 2*99+2*97
for(i in 2:(nrow(input)-1)){
  for(j in 2:(ncol(input)-1)){
    height <- input[i,j]
    if(all(input[1:(i-1),j]<height)|
       all(input[(i+1):nrow(input),j]<height)|
       all(input[i,1:(j-1)]<height)|
       all(input[i,(j+1):ncol(input)]<height)
       )
      n <- n+1
  }
}
n
#1763

#Alt part 1
map <- input
map[]<-FALSE
map[1,]<-TRUE
map[nrow(map),] <- TRUE
map[,1] <- TRUE
map[,ncol(map)] <- TRUE
for(i in 2:(nrow(input)-1)){
  for(j in 2:(ncol(input)-1)){
    height <- input[i,j]
    if(all(input[1:(i-1),j]<height)|
       all(input[(i+1):nrow(input),j]<height)|
       all(input[i,1:(j-1)]<height)|
       all(input[i,(j+1):ncol(input)]<height)
    )
      map[i,j] <- TRUE
  }
}
sum(map)
#1763

map2 <- input
map2[]<-0
for(i in 2:(nrow(input)-1)){
  for(j in 2:(ncol(input)-1)){
    height <- input[i,j]
    w <- which(input[(i-1):1,j]>=height)
    scenic  <- ifelse(length(w),w[1],i-1)
    w <- which(input[(i+1):nrow(input),j]>=height)
    scenic  <- scenic * ifelse(length(w),w[1],nrow(input)-i)
    w <- which(input[i,(j-1):1]>=height)
    scenic  <- scenic * ifelse(length(w),w[1],j-1)
    w <- which(input[i,(j+1):ncol(input)]>=height)
    scenic  <- scenic * ifelse(length(w),w[1],ncol(input)-j)
    map2[i,j] <- scenic
  }
}
max(map2)
#671160

### Visualization
png("plot_day08.png",h=400,w=1060)
par(mfcol=c(1,3))
par(mar=c(0,0,5,0))
image(input,col=rev(hcl.colors(10,"ag_GrnYl")),ax=F,ann=F,asp=1)
box(lwd=3)
title("Tree Height")

par(mar=c(0,0,5,0))
image(map,col=c("white","black"),ax=F,ann=F,asp=1)
box(lwd=3)
title("Tree Visibility")

par(mar=c(0,0,5,0))
image(1:99,1:99,log(map2),col=rev(hcl.colors(10,"ag_GrnYl")),ax=F,ann=F,asp=1)
points(48,79,pch=15,col="red")
box(lwd=3)
title("Tree Scenic Score (Log)")
dev.off()

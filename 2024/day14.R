input <- readLines(read.input(14))
robots <-parse.group("p=(?<px>[-0-9]+),(?<py>[-0-9]+) v=(?<vx>[-0-9]+),(?<vy>[-0-9]+)",input)
robots <- as.data.frame(apply(robots,2,as.integer))
nc <- 101
nr <- 103
pos <- list()
for(i in 1:100){
  for(j in 1:500){
    if(i==1){
      npx <- (robots$px[j]+robots$vx[j])%%nc
      npy <- (robots$py[j]+robots$vy[j])%%nr
    }else{
      npx <- (pos[[i-1]][j,1]+robots$vx[j])%%nc
      npy <- (pos[[i-1]][j,2]+robots$vy[j])%%nr
    }
    if(j==1){
      pos[[i]] <- matrix(c(npx,npy),ncol=2,nrow=1)
    }else{
      pos[[i]] <- rbind(pos[[i]],c(npx,npy))
    }
  }
}
lp <- pos[[100]]
q1 <- lp[lp[,1]<=49&lp[,2]<=50,]
q2 <- lp[lp[,1]<=49&lp[,2]>=52,]
q3 <- lp[lp[,1]>=51&lp[,2]<=50,]
q4 <- lp[lp[,1]>=51&lp[,2]>=52,]
nrow(q1)*nrow(q2)*nrow(q3)*nrow(q4)
#226548000

pdf("day14.pdf")
pos <- list()
for(i in 1:10000){
    for(j in 1:500){
      if(i==1){
        npx <- (robots$px[j]+robots$vx[j])%%nc
        npy <- (robots$py[j]+robots$vy[j])%%nr
      }else{
        npx <- (pos[[i-1]][j,1]+robots$vx[j])%%nc
        npy <- (pos[[i-1]][j,2]+robots$vy[j])%%nr
      }
      if(j==1){
        pos[[i]] <- matrix(c(npx,npy),ncol=2,nrow=1)
      }else{
        pos[[i]] <- rbind(pos[[i]],c(npx,npy))
      }
    }
  map <- matrix(0,nrow=nr,ncol=nc)
  for(j in 1:500){
    map[pos[[i]][j,2]+1,pos[[i]][j,1]+1] <- map[pos[[i]][j,2]+1,pos[[i]][j,1]+1]+1
  }
  par(mar=c(0,0,0,0))
  image(map)
  if(!i%%100)cat(i,"\r")
}
dev.off()

#7753
#Part1
options(digits=22)
input <- readLines("input20.txt")
#input <- readLines("test20.txt")
tiles <- list()
n <- 1
for(i in 1:144){
  tiles[[i]] <- list()
  tiles[[i]]$tile <- do.call(rbind,strsplit(input[n+1:10],""))
  tiles[[i]]$nb <- as.integer(gsub("^Tile ([0-9]+):$","\\1",input[n]))
  n <- n+12
}

sides <- lapply(tiles,function(x)c(paste(x$tile[1,],collapse=""),
                                   paste(x$tile[,10],collapse=""),
                                   paste(x$tile[10,],collapse=""),
                                   paste(x$tile[,1],collapse="")))
n_matching_sides <- c()
for(i in 1:144){
  n_matching_sides[i]<-sum(sapply(sides[[i]],function(x)x%in%unlist(sides[-i])|intToUtf8(rev(utf8ToInt(x)))%in%unlist(sides[-i])))
}
prod(sapply(tiles[which(n_matching_sides==2)],function(x)x$nb))
#15003787688423

#Part2 (result do not match so far)
for(i in 1:144){
  tiles[[i]]$sides <- sides[[i]]
  tiles[[i]]$matching_sides <- sapply(sides[[i]],function(x)x%in%unlist(sides[-i])|intToUtf8(rev(utf8ToInt(x)))%in%unlist(sides[-i]))
  tiles[[i]]$n_matching <- n_matching_sides[i]
}
for(i in 1:144){
  y <- tiles[[i]]$sides
  mat <- lapply(tiles[-i],function(X){
    m<-sapply(X$sides,function(x)x==y|intToUtf8(rev(utf8ToInt(x)))==y)
    if(any(m)){
      return(cbind(X$nb,which(m,arr.ind=T)))
      }else{return(NULL)}
    })
  tiles[[i]]$matched <- do.call(rbind,mat)
}

reorient <- function(tile,side,previous,from="left"){
  if(from=="left"){
    w <- which(c(tile$side[side]==previous$side[2],intToUtf8(rev(utf8ToInt(tile$side[side])))==previous$side[2]))
    if((side==4&from=="left")|(side==1&from=="top")){
      if(w!=1){
        tile$tile <- tile$tile[10:1,]
      }
    }
    if(side==2){
      if(w==1){
        tile$tile <- tile$tile[,10:1]
      }else{
        tile$tile <- tile$tile[10:1,10:1]
      }
    }
    if(side==1){
      if(w==1){
        tile$tile <- t(tile$tile)
      }else{
        tile$tile <- t(tile$tile)[10:1,]
      }
    }
    if(side==3){
      if(w==1){
        tile$tile <- t(tile$tile)[10:1,10:1]
      }else{
        tile$tile <- t(tile$tile)[,10:1]
      }
    }
  }
  if(from=="top"){
    w <- which(c(tile$side[side]==previous$side[3],intToUtf8(rev(utf8ToInt(tile$side[side])))==previous$side[3]))
    if(side==1){
      if(w!=1){
        tile$tile <- tile$tile[,10:1]
      }
    }
    if(side==2){
      if(w==1){
        tile$tile <- t(tile$tile)[10:1,]
      }else{
        tile$tile <- t(tile$tile)[10:1,10:1]
      }
    }
    if(side==3){
      if(w==1){
        tile$tile <- tile$tile[10:1,]
      }else{
        tile$tile <- tile$tile[10:1,10:1]
      }
    }
    if(side==4){
      if(w==1){
        tile$tile <- t(tile$tile)
      }else{
        tile$tile <- t(tile$tile)[,10:1]
      }
    }
  }
  tile$sides <- c(paste(tile$tile[1,],collapse=""), 
                  paste(tile$tile[,10],collapse=""), 
                  paste(tile$tile[10,],collapse=""),
                  paste(tile$tile[,1],collapse=""))
  mat <- lapply(tiles,function(X){
      m<-sapply(X$sides,function(x)x==tile$sides|intToUtf8(rev(utf8ToInt(x)))==tile$sides)
      if(any(m)){
        return(cbind(X$nb,which(m,arr.ind=T)))
      }else{return(NULL)}
    })
  mat <- do.call(rbind,mat)
  tile$matched <- mat[mat[,1]!=tile$nb,]
  tile
}

map <- matrix(nrow=8*12,ncol=8*12)
nbs <- sapply(tiles,function(x)x$nb)
used <- matrix(nr=12,nc=12)
modtiles <- tiles
for(j in 1:12){
  if(j==1){
    ind <- tiles[which(n_matching_sides==2)][[1]]$nb
    first_tile <- tiles[nbs==ind][[1]]
    first_tile$tile <- first_tile$tile[10:1,10:1]
    first_tile$sides <- c(paste(first_tile$tile[1,],collapse=""), 
                    paste(first_tile$tile[,10],collapse=""), 
                    paste(first_tile$tile[10,],collapse=""),
                    paste(first_tile$tile[,1],collapse=""))
    mat <- lapply(tiles,function(X){
      m<-sapply(X$sides,function(x)x==first_tile$sides|intToUtf8(rev(utf8ToInt(x)))==first_tile$sides)
      if(any(m)){
        return(cbind(X$nb,which(m,arr.ind=T)))
      }else{return(NULL)}
    })
    mat <- do.call(rbind,mat)
    first_tile$matched <- mat[mat[,1]!=first_tile$nb,]
    map[1:8,1:8]<-first_tile$tile[2:9,2:9]
    used[1,1]<-ind
    lasttile <-first_tile
    modtiles[nbs==lasttile$nb][[1]] <- lasttile
    for(i in 1:11){
      m <- lasttile$matched
      M <- m[m[,2]==2,,drop=FALSE]
      M <- M[!M[,1]%in%used,]
      nexttile <- tiles[nbs==M[1]][[1]]
      nexttile <- reorient(nexttile,M[3],lasttile,"left")
      map[(j-1)*8+1:8,i*8+1:8]<-nexttile$tile[2:9,2:9]
      used[j,i+1] <- nexttile$nb
      lasttile <- nexttile
      modtiles[nbs==lasttile$nb][[1]] <- lasttile
    }
  }else{
    top <- used[j-1,1]
    lasttile <- modtiles[nbs==top][[1]]
    m <- lasttile$matched
    M <- m[m[,2]==3,,drop=FALSE]
    M <- M[!M[,1]%in%used,]
    nexttile <- tiles[nbs==M[1]][[1]]
    nexttile <- reorient(nexttile,M[3],lasttile,"top")
    map[(j-1)*8+1:8,1:8]<-nexttile$tile[2:9,2:9]
    used[j,1] <- nexttile$nb
    lasttile <- nexttile
    modtiles[nbs==lasttile$nb][[1]] <- lasttile
    for(i in 1:11){
      m <- lasttile$matched
      top <- used[j-1,i+1]
      m2 <- modtiles[nbs==top][[1]]$matched
      M <- m[m[,1]%in%m2[,1],,drop=FALSE]
      M <- M[!M[,1]%in%used,]
      nexttile <- tiles[nbs==M[1]][[1]]
      nexttile <- reorient(nexttile,M[3],lasttile,"left")
      map[(j-1)*8+1:8,i*8+1:8]<-nexttile$tile[2:9,2:9]
      used[j,i+1] <- nexttile$nb
      lasttile <- nexttile
      modtiles[nbs==lasttile$nb][[1]] <- lasttile
    }
  }
}
write(t(map),ncolumns=96,file="visualisations/map20.txt",sep="")
monster <- "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
monster <- do.call(rbind,strsplit(el(strsplit(monster,"\n")),""))
mask <- monster=="#"
map2 <- map
for(i in 1:94){
  for(j in 1:77){
    if(all(map[i+0:2,j+0:19][mask]=="#")) map2[i+0:2,j+0:19][mask]<-"O"
    if(all(map[i+0:2,j+0:19][mask[,20:1]]=="#")) map2[i+0:2,j+0:19][mask[,20:1]]<-"O"
    if(all(map[i+0:2,j+0:19][mask[3:1,]]=="#")) map2[i+0:2,j+0:19][mask[3:1,]]<-"O"
    if(all(map[i+0:2,j+0:19][mask[3:1,20:1]]=="#")) map2[i+0:2,j+0:19][mask[3:1,20:1]]<-"O"
    if(all(map[j+0:19,i+0:2][t(mask)]=="#")) map2[j+0:19,i+0:2][t(mask)]<-"O"
    if(all(map[j+0:19,i+0:2][t(mask)[20:1,]]=="#")) map2[j+0:19,i+0:2][t(mask)[20:1,]]<-"O"
    if(all(map[j+0:19,i+0:2][t(mask)[,3:1]]=="#")) map2[j+0:19,i+0:2][t(mask)[,3:1]]<-"O"
    if(all(map[j+0:19,i+0:2][t(mask)[20:1,3:1]]=="#")) map2[j+0:19,i+0:2][t(mask)[20:1,3:1]]<-"O"
  }
}
sum(map2=="#")
#1900
map3<-matrix(as.integer(factor(map2)),nr=96,nc=96)
png("visualisations/map20.png",w=800,h=800)
par(mar=c(0,0,0,0))
image(t(map3),col=c("lightblue","blue","red"),asp=1,ann=F,ax=F)
dev.off()

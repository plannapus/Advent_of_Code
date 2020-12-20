#Part1
options(digits=22)
input <- readLines("input20.txt")
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

#Part2
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
      if(w==1){
        ord <- order(1:4)
      }else{
        tile$tile <- tile$tile[10:1,]
        ord <- order(c(3,2,1,4))
      }
    }
    if(side==2){
      if(w==1){
        tile$tile <- tile$tile[,10:1]
        ord <- order(c(1,4,3,2))
      }else{
        tile$tile <- tile$tile[10:1,10:1]
        ord <- order(c(3,4,1,2))
      }
    }
    if(side==1){
      if(w==1){
        tile$tile <- t(tile$tile)
        ord <- order(c(2,3,4,1))
      }else{
        tile$tile <- t(tile$tile)[10:1,]
        ord <- order(c(4,3,2,1))
      }
    }
    if(side==3){
      if(w==1){
        tile$tile <- t(tile$tile)[,10:1]
        ord <- order(c(2,1,4,3))
      }else{
        tile$tile <- t(tile$tile)[10:1,10:1]
        ord <- order(c(4,1,2,3))
      }
    }
  }
  if(from=="top"){
    w <- which(c(tile$side[side]==previous$side[3],intToUtf8(rev(utf8ToInt(tile$side[side])))==previous$side[3]))
    if(side==1){
      if(w==1){
        ord <- 1:4
      }else{
        tile$tile <- tile$tile[,10:1]
        ord <- order(c(1,4,3,2))
      }
    }
    if(side==2){
      if(w==1){
        tile$tile <- t(tile$tile[10:1,])
        ord <- order(c(2,3,4,1))
      }else{
        tile$tile <- t(tile$tile[10:1,10:1])
        ord <- order(c(2,1,4,3))
      }
    }
    if(side==3){
      if(w==1){
        tile$tile <- tile$tile[10:1,]
        ord <- order(c(3,2,1,4))
      }else{
        tile$tile <- tile$tile[10:1,10:1]
        ord <- order(c(3,4,1,2))
      }
    }
    if(side==4){
      if(w==1){
        tile$tile <- t(tile$tile)
        ord <- order(c(4,3,2,1))
      }else{
        tile$tile <- t(tile$tile)[,10:1]
        ord <- order(c(4,1,2,3))
      }
    }
  }
  tile$sides <- tile$sides[ord]
  tile$matched[,2] <- ord[tile$matched[,2]]
  tile
}

map <- matrix(nrow=120,ncol=120)

nbs <- sapply(tiles,function(x)x$nb)
ind <- tiles[which(n_matching_sides==2)][[1]]$nb
first_tile <- tiles[nbs==ind][[1]]
first_tile$tile <- first_tile$tile[10:1,10:1]
first_tile$sides <- first_tile$sides[order(c(3,4,1,2))]
first_tile$matched[,2] <- order(c(3,4,1,2))[first_tile$matched[,2]]
map[1:10,1:10]<-first_tile$tile
used <- ind
lasttile <-first_tile
for(i in 1:11){ #First row
  m <- lasttile$matched
  M <- m[m[,2]==2,,drop=FALSE]
  M <- M[!M[,1]%in%used,]
  if(length(M)>3)stop()
  nexttile <- tiles[nbs==M[1]][[1]]
  cat(nexttile$n_matching,"\n")
  nexttile <- reorient(nexttile,M[3],lasttile,"left")
  map[1:10,i*10+1:10]<-nexttile$tile
  used <- c(used,nexttile$nb)
  lasttile <- nexttile
}


#[...]

cutmap <- map[-c((0:11)*10+1,(1:12)*10),-c((0:11)*10+1,(1:12)*10)]
monster <- "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
monster <- do.call(rbind,strsplit(el(strsplit(monster,"\n")),""))
pattern <- which(monster=="#",arr.ind=TRUE)

# map2 <- cutmap
# n <- 0
# for(i in 0:93){
#   for(j in 0:76){
#     n <- n + all(apply(pattern,1,function(x)cutmap[i+x[1],j+x[2]]=="#"))
#     if(all(apply(pattern,1,function(x)cutmap[i+x[1],j+x[2]]=="#"))){
#       for(k in 1:nrow(pattern)) map2[i+pattern[k,1],j+pattern[k,2]] <- "O"
#     }
#   }
# }
# 
# cutmap[cutmap=="#"]<-1
# cutmap[cutmap=="."]<-0
# cutmap<-apply(cutmap,2,as.integer)
# image(t(cutmap))

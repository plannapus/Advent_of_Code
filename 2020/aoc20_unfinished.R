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
                          paste(x$tile[10,],collapse=""),
                          paste(x$tile[,1],collapse=""),
                          paste(x$tile[,10],collapse="")))
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

orient <- function(next_tile,w,side="right"){
  if(side=="right"){
    if(w[1]==1) next_tile <- t(next_tile[10:1,])
    if(w[1]==2) next_tile <- t(next_tile[,10:1])
    if(w[1]==4) next_tile <- next_tile[,10:1]
    if(w[1]==5) next_tile <- t(next_tile)
    if(w[1]==6) next_tile <- t(next_tile[10:1,10:1])
    if(w[1]==7) next_tile <- next_tile[10:1,]
    if(w[1]==8) next_tile <- next_tile[10:1,10:1]
  }else if(side=="bottom"){
    if(w[1]==4) next_tile <- t(next_tile[10:1,])
    if(w[1]==7) next_tile <- t(next_tile[,10:1])
    if(w[1]==5) next_tile <- next_tile[,10:1]
    if(w[1]==3) next_tile <- t(next_tile)
    if(w[1]==8) next_tile <- t(next_tile[10:1,10:1])
    if(w[1]==2) next_tile <- next_tile[10:1,]
    if(w[1]==6) next_tile <- next_tile[10:1,10:1]
  }
  next_tile
}

map <- matrix(nrow=120,ncol=120)

# first_tile <- tiles[which(n_matching_sides==2)][[1]]$tile
# fs <- sides[which(n_matching_sides==2)][[1]]
# W <- which(sapply(fs,function(x)x%in%unlist(sides[which(n_matching_sides>2)])|intToUtf8(rev(utf8ToInt(x)))%in%unlist(sides[which(n_matching_sides>2)])))
# map[1:10,1:10]<-first_tile[10:1,10:1]
# used <- tiles[which(n_matching_sides==2)][[1]]$nb
# for(i in 1:10){
#   sidetile <- tiles[n_matching_sides==3&!sapply(tiles,function(x)x$nb)%in%used]
#   s <- sides[n_matching_sides==3&!sapply(tiles,function(x)x$nb)%in%used]
#   w <- which(sapply(s,function(x)c(x,sapply(x,function(X)intToUtf8(rev(utf8ToInt(X)))))%in%paste(map[1:10,i*10],collapse="")),arr.ind=T)
#   if(length(w)>2)stop()
#   next_tile <- sidetile[[w[2]]]$tile
#   used <- c(used,sidetile[[w[2]]]$nb)
#   map[1:10,(i*10)+1:10] <- orient(next_tile,w,"right")
# }
# for(j in 1:11){
#   sidetile <- tiles[which(n_matching_sides<4)]
#   s <- sides[which(n_matching_sides<4)]
#   w <- which(sapply(s,function(x)c(x,sapply(x,function(X)intToUtf8(rev(utf8ToInt(X)))))%in%paste(map[j*10,1:10],collapse="")),arr.ind=T)
#   next_tile <- sidetile[[w[2]]]$tile
#   used <- c(used,sidetile[[w[2]]]$nb)
#   map[(j*10)+1:10,1:10] <- orient(next_tile,w,"bottom")
# }
# for(i in 1:11){
#   for(j in 1:11){
#     w <- which(sapply(sides,function(x)c(x,sapply(x,function(X)intToUtf8(rev(utf8ToInt(X)))))%in%paste(map[(j*10)+1:10,i*10],collapse="")),arr.ind=T)
#     next_tile <- tiles[[w[2]]]$tile
#     used <- c(used,tiles[[w[2]]]$nb)
#     map[(j*10)+1:10,(i*10)+1:10] <- orient(next_tile,w,"right")
#   }
# }
# cutmap <- map[-c((0:11)*10+1,(1:12)*10),-c((0:11)*10+1,(1:12)*10)]

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

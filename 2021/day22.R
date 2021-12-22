input <- readLines("input22.txt")
coords <- apply(do.call(rbind,regmatches(input,gregexpr("-?[0-9]+",input))),2,as.integer)
io <- grepl("^on",input)
m <- array(FALSE,dim=c(101,101,101))
for(i in 1:nrow(coords)){
  m[(-50:50)%in%coords[i,1]:coords[i,2],
    (-50:50)%in%coords[i,3]:coords[i,4],
    (-50:50)%in%coords[i,5]:coords[i,6]]<-io[i]
}
sum(m)
#603661

cubes <- list()
for(i in 1:nrow(coords)){
  cube <- c(coords[i,],as.integer(io[i]))
  cube[c(2,4,6)]<-cube[c(2,4,6)]+1
  new_cube <- list()
  for(j in seq_along(cubes)){
    cb <- cubes[[j]]
    xo <- cube[2]>cb[1] & cube[1]<cb[2]
    yo <- cube[4]>cb[3] & cube[3]<cb[4]
    zo <- cube[6]>cb[5] & cube[5]<cb[6]
    if(xo & yo & zo){
      if(cb[1] < cube[1]){
        new <- cb
        new[2] <- cube[1]
        cb[1] <- cube[1]
        new_cube[[length(new_cube)+1]] <- new
      }
      if(cb[2] > cube[2]){
        new <- cb
        new[1] <- cube[2]
        cb[2] <- cube[2]
        new_cube[[length(new_cube)+1]] <- new
      }
      if(cb[3] < cube[3]){
        new <- cb
        new[4] <- cube[3]
        cb[3] <- cube[3]
        new_cube[[length(new_cube)+1]] <- new
      }
      if(cb[4] > cube[4]){
        new <- cb
        new[3] <- cube[4]
        cb[4] <- cube[4]
        new_cube[[length(new_cube)+1]] <- new
      }
      if(cb[5] <= cube[5]){
        new <- cb
        new[6] <- cube[5]
        cb[5] <- cube[5]
        new_cube[[length(new_cube)+1]] <- new
      }
      if(cb[6] > cube[6]){
        new <- cb
        new[5] <- cube[6]
        cb[6] <- cube[6]
        new_cube[[length(new_cube)+1]] <- new
      }
    }else{
      new_cube[[length(new_cube)+1]] <- cb
    }
  }
  new_cube[[length(new_cube)+1]] <- cube
  cubes <- new_cube
}
CB <- do.call(rbind,cubes)
CB <- CB[CB[,7]==1,]

a <- CB[,2]-CB[,1]
b <- CB[,4]-CB[,3]
d <- CB[,6]-CB[,5]
a <- as.integer64(a)
b <- as.integer64(b)
d <- as.integer64(d)
sum(a*b*d)
#1237264238382479
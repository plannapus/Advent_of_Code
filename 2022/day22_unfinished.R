input <- readLines("input22.txt")
s<-strsplit(input[1:200],"")
map <- do.call(rbind,lapply(s,function(x) if(length(x)!=150){
  return(c(x,rep(" ",150-length(x))))
  }else{return(x)}))
inst <- el(strsplit(input[202],"(?<=\\d)(?=\\D)|(?=\\d)(?<=\\D)",perl=TRUE))
loc <- c(1,which(map[1,]==".")[1])
d <- ">"
correct <- function(new){
  if(new[2]>ncol(map)) new[2] <- 1
  if(new[2]<1) new[2] <- ncol(map)
  if(new[1]>nrow(map)) new[1] <- 1
  if(new[1]<1) new[1] <- nrow(map)
  new
}
path <- cbind(rep(NA,4002),rep(NA,4002))
path[1,]<-loc
options(warn=-1)
for(i in seq_along(inst)){
  if(!is.na(as.integer(inst[i]))){
    n <- as.integer(inst[i])
    incr <- switch(d,"<"=c(0,-1),"v"=c(1,0),">"=c(0,1),"^"=c(-1,0))
    for(k in 1:n){
      new <- loc+incr
      new <- correct(new)
      while(map[new[1],new[2]]==" "){
        new <- new+incr
        new <- correct(new)
      }
      if(map[new[1],new[2]]=="#") break
      loc <- new
    }
  }else{
    if(inst[i]=="R"){
      dirs <- c("<","^",">","v","<")
      d <- dirs[which(dirs==d)[1]+1]
    }else{
      dirs <- c("<","v",">","^","<")
      d <- dirs[which(dirs==d)[1]+1]
    }
  }
  path[i+1,]<-loc
  cat(i,":",loc[1],"-",loc[2],"\n")
}

loc[1]*1000+loc[2]*4+switch(d,"<"=2,"^"=3,">"=0,"v"=1)
#149138

### Part 2 in progress
map_dice <- matrix(0,ncol=150,nrow=200)
map_dice[1:50,51:100]<-1
map_dice[1:50,101:150]<-2
map_dice[51:100,51:100]<-3
map_dice[101:150,1:50]<-5
map_dice[101:150,51:100]<-4
map_dice[151:200,1:50]<-6

#-12
#-3-
#54-
#6--

loc <- c(1,which(map[1,]==".")[1])
d <- ">"
correct_dice <- function(loc,new,d){
  md1 <- map_dice[loc[1],loc[2]]
  if(new[2]>ncol(map)&md1==2){
    #Go to Side 4
    d <- "<"
    mappingXX <- cbind(1:50,150:101)
    newC <- c(mappingXX[mappingXX[,1]==new[1],2],100)
    new <- newC
  }
  if(new[2]<1){
    if(md1==5){
      #Go to Side 1
      d <- ">"
      mappingXX <- cbind(150:101,1:50)
      newC <- c(mappingXX[mappingXX[,1]==new[1],2],51)
      new <- newC
    }
    if(md1==6){
      #Go to Side 1
      d <- "v"
      mappingXY <- cbind(151:200,51:100)
      newC <- c(1,mappingXY[mappingXY[,1]==new[1],2])
      new <- newC
    }
  }
  if(new[1]<1){
    if(md1==1){
      #Go to Side 6
      d <- ">"
      mappingYX <- cbind(51:100,151:200)
      newC <- c(mappingYX[mappingYX[,1]==new[2],2],1)
      new <- newC
    }
    if(md1==2){
      #Go to Side 6
      d <- "^"
      mappingYY <- cbind(101:150,1:50)
      newC <- c(200,mappingYY[mappingYY[,1]==new[2],2])
      new <- newC
    }
  }
  if(new[1]>nrow(map)&md1==6){
    # Go to Side 2
    dir <- "v"
    mappingYY <- cbind(1:50,101:150)
    newC <- c(1,mappingYY[mappingYY[,1]==new[2],2])
    new <- newC
  }
  if(map[new[1],new[2]]==" "){
    if(md1==1){
      #Go to Side 5
      d <- ">"
      mappingXX <- cbind(1:50,150:101)
      newC <- c(mappingXX[mappingXX[,1]==new[1],2],1)
      new <- newC
    }
    if(md1==2){
      #Go to Side 3
      d <- "<"
      mappingYX <- cbind(101:150,51:100)
      newC <- c(mappingYX[mappingYX[,1]==new[2],2],100)
      new <- newC
    }
    if(md1==3){
      w<-which(map_dice==3,arr.ind=T)
      if(new[2]<min(w[,2])){
        #Go to Side 5
        d <- "v"
        mappingXY <- cbind(51:100,1:50)
        newC <- c(101,mappingXY[mappingXY[,1]==new[1],2])
        new <- newC
      }else{
        #Go to Side 2
        d <- "^"
        mappingXY <- cbind(51:100,101:150)
        newC <- c(50,mappingXY[mappingXY[,1]==new[1],2])
        new <- newC
      }
    }
    if(md1==4){
      w<-which(map_dice==4,arr.ind=T)
      if(new[2]>max(w[,2])){
        #Go to Side 2
        d <- "<"
        mappingXX <- cbind(101:150,50:1)
        newC <- c(mappingXX[mappingXX[,1]==new[1],2],150)
        new <- newC
      }else{
        #Go to Side 6
        d <- "<"
        mappingYX <- cbind(51:100,151:200)
        newC <- c(mappingYX[mappingYX[,1]==new[2],2],50)
        new <- newC
      }
    }
    if(md1==5){
     #Go to Side 3
      d<-">"
      mappingYX <- cbind(1:50,51:100)
      newC <- c(mappingYX[mappingYX[,1]==new[2],2],51)
      new <- newC
    }
    if(md1==6){
      #Go to side 4
      d <- "^"
      mappingXY <- cbind(151:200,51:100)
      newC <- c(150,mappingXY[mappingXY[,1]==new[1],2])
      new <- newC
    }
  }
  if(map[new[1],new[2]]==" ") stop()
  return(list(new=new,d=d))
}
path <- cbind(rep(NA,4002),rep(NA,4002))
path[1,]<-loc
options(warn=-1)
for(i in seq_along(inst)){
  if(!is.na(as.integer(inst[i]))){
    n <- as.integer(inst[i])
    for(k in 1:n){
      incr <- switch(d,"<"=c(0,-1),"v"=c(1,0),">"=c(0,1),"^"=c(-1,0))
      new <- loc+incr
      NEW <- correct_dice(loc,new,d)
      new <- NEW$new
      d <- NEW$d
      if(map[new[1],new[2]]==".") loc <- new
    }
  }else{
    if(inst[i]=="R"){
      dirs <- c("<","^",">","v","<")
      d <- dirs[which(dirs==d)[1]+1]
    }else{
      dirs <- c("<","v",">","^","<")
      d <- dirs[which(dirs==d)[1]+1]
    }
  }
  path[i+1,]<-loc
  cat(i,":",loc[1],"-",loc[2],"\n")
}

loc[1]*1000+loc[2]*4+switch(d,"<"=2,"^"=3,">"=0,"v"=1)
#115106 #<-Incorrect!
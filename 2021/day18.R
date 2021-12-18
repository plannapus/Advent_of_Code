add <- function(a,b) paste0("[",a,",",b,"]")
split <- function(x){
  if(any(grepl("[0-9]{2}",x))){
    g <- gregexpr("[0-9]{2}",x)
    r <- el(regmatches(x,g))[1]
    h <- as.integer(r)/2
    s <- paste0("[",floor(h),",",ceiling(h),"]")
    x <- paste0(substr(x,1,g[[1]][1]-1),
                s,
                substr(x,g[[1]][1]+attr(g[[1]],"match.length")[1],nchar(x)))
  }
 x
}
nestedness <- function(x){
  n <- nchar(x)
  level <- -1
  nest <- rep(0,n)
  for(i in seq_len(n)){
    if(substr(x,i,i)=="[") level <- level+1
    nest[i] <- level
    if(substr(x,i,i)=="]") level <- level-1
  }
  nest
}
is_digit <- function(x) !is.na(as.integer(el(strsplit(x,""))))
explode <- function(x){
  n <- nestedness(x)
  w <- integer(0)
  if(max(n)>=4) w <- which(n==max(n))
  if(length(w)){
    i1 <- 1
    i2 <- ifelse(length(w)>7,head(which(diff(w)>1),1),length(w))
    s <- substr(x,w[i1],w[i2])
    to_add <- as.integer(unlist(regmatches(s,gregexpr("[0-9]+",s))))
    d <- which(is_digit(x))
    d_left <- tail(d[d<w[i1]],1)
    d_right <- head(d[d>w[i2]],1)
    if(length(d_left)){
      if(any(d==d_left-1)){
        s1 <- as.integer(substr(x,d_left-1,d_left))+to_add[1]
        p1 <- paste0(substr(x,1,d_left-2),s1,substr(x,d_left+1,w[i1]-1))
      }else{
        s1 <- as.integer(substr(x,d_left,d_left))+to_add[1]
        p1 <- paste0(substr(x,1,d_left-1),s1,substr(x,d_left+1,w[i1]-1))
      }
    }else{
      p1 <- paste0(substr(x,1,w[i1]-1),0)
      }
    if(length(d_right)){
      if(any(d==d_right+1)){
        s2 <- as.integer(substr(x,d_right,d_right+1))+to_add[2]
        p2 <- paste0(substr(x,w[i2]+1,d_right-1),s2,substr(x,d_right+2,nchar(x)))  
      }else{
        s2 <- as.integer(substr(x,d_right,d_right))+to_add[2]
        p2 <- paste0(substr(x,w[i2]+1,d_right-1),s2,substr(x,d_right+1,nchar(x)))      
      }
    }else{
      p2 <- paste0(0,substr(x,w[i2]+1,nchar(x)))
      }
    x <- paste0(p1,p2)
    x <- gsub("\\[,","[0,",gsub(",\\]",",0]",x))
  }
  x
}
input <- readLines("input18.txt")
#input <- readLines("tests/test18_2.txt")
for(i in seq_along(input)){
  if(i==1) snail <- input[i]
  if(i!=1){
    snail <- add(snail, input[i])
    repeat{
      s <- snail
      repeat{
        s1 <- snail
        snail <- explode(snail)
        #cat(snail,"\n")
        if(s1==snail) break
      }
      snail <- split(snail)
      #cat(snail,"\n")
      if(snail==s) break
    }
  }
  cat(snail,"\n")
}
magnitude <- function(x){
  repeat{
    y <- x
    g <- gregexpr("\\[([0-9]+),([0-9]+)\\]",x)
    innermost <- el(regmatches(x,g))
    if(length(innermost)){
      s<-3*as.integer(gsub("\\[([0-9]+),.+$","\\1",innermost[1]))+2*as.integer(gsub("^.+,([0-9]+)\\]$","\\1",innermost[1]))
      x <- paste0(substr(x,1,g[[1]][1]-1),s,substr(x,g[[1]][1]+attr(g[[1]],"match.length")[1],nchar(x)))
    }
    if(x==y) break
  }
  as.integer(x)
}
magnitude(snail)
#3987

eg <- expand.grid(1:100,1:100)
magn <- c()
eg <- eg[eg[,1]!=eg[,2],]
for(i in 1:nrow(eg)){
  snail <- add(input[eg[i,1]], input[eg[i,2]])
  repeat{
    s <- snail
    repeat{
      s1 <- snail
      snail <- explode(snail)
      if(s1==snail) break
    }
    snail <- split(snail)
    if(snail==s) break
  }
  magn[i] <- magnitude(snail)
  if(!i%%100)cat(i,"\r")
}
max(magn)
#4500
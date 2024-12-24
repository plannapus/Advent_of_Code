source("read.input.R")
input <- readLines(read.input(21))
#input <- c("029A","980A","179A","456A","379A")
inp <- strsplit(input,"")

disp1 <- matrix(c(7:9,4:6,1:3,"",0,"A"),ncol=3,byrow=TRUE)
#789
#456
#123
# 0A
disp2 <- matrix(c("","^","A","<","v",">"),ncol=3,byrow=TRUE)
# >A
#<v>


find_disp <- \(x,dsp,length.only=FALSE){
  click <- list()
  for(i in seq_along(x)){
    n <- x[i]
    p <- ifelse(i==1,"A",x[i-1])
    if(dsp==1){
      w1 <- which(disp1==n,arr.ind=TRUE)
      w2 <- which(disp1==p,arr.ind=TRUE)
    }else{
      w1 <- which(disp2==n,arr.ind=TRUE)
      w2 <- which(disp2==p,arr.ind=TRUE)
    }
    d <- w2-w1
    k <- c()
    if(d[1]>0) k <- c(k,rep("^",d[1]))
    if(d[1]<0) k <- c(k,rep("v",abs(d[1])))
    if(d[2]>0) k <- c(k,rep("<",d[2]))
    if(d[2]<0) k <- c(k,rep(">",abs(d[2])))
    if(length(unique(k))>1){
      K <- gtools::permutations(length(k),length(k),k,set=FALSE)
      click[[i]] <- K[!duplicated(K),]
    }else if(is.null(k)){
      click[[i]] <- matrix(nrow=1,ncol=0)
    }else{
      click[[i]] <- matrix(k,nrow=1)
    }
  }
  for(i in 1:length(x)){
    click[[i]] <- cbind(click[[i]],"A")
  }
  if(length.only==TRUE){
    return(sum(sapply(click,ncol)))
  }
  for(i in 2:length(x)){
    for(j in 1:nrow(click[[i]])){
      if(j==1) cl <- click[[i-1]]
      if(j>1) cl <- rbind(cl,click[[i-1]])
    }
    if(ncol(click[[i]])){
      for(k in 1:ncol(click[[i]])){
        cl <- cbind(cl,rep(click[[i]][,k],each=nrow(click[[i-1]])))
      }
    }
    cl <- cl[apply(cl,1,path_possible,dsp=dsp),,drop=FALSE]
    cl <- cl[!duplicated(cl),,drop=FALSE]
    #cat(i,":",nrow(cl),"\r")
    click[[i]] <- cl
  }
  res <- click[[length(click)]]
  l <- list()
  for(i in 1:nrow(res)){
    l[[i]] <- res[i,]
  }
  l[sapply(l,length)>0]
}

path_possible <- \(x,dsp){
  if(dsp==1){
    start <- c(4,3)
    nogo <- c(4,1)
  }else{
    start <- c(1,3)
    nogo <- c(1,1)
  }
  pos <- start
  for(j in seq_along(x)){
    if(x[j]=="^") pos <- pos+c(-1,0)
    if(x[j]=="v") pos <- pos+c(1,0)
    if(x[j]=="<") pos <- pos+c(0,-1)
    if(x[j]==">") pos <- pos+c(0,1)
    if(pos[1]==nogo[1]&pos[2]==nogo[2]) return(FALSE)
  }
  return(TRUE)
}

s <- c()
for(i in 1:5){
  r1 <- find_disp(inp[[i]],1)
  r2 <- unlist(lapply(r1,find_disp,2),recursive=FALSE)
  r2 <- r2[sapply(r2,length)==min(sapply(r2,length))]
  l <- sapply(r2,find_disp,2,length.only=TRUE)
  s[i] <- as.integer(substr(input[i],1,3))*min(l)
  # r3 <- unlist(lapply(r2,find_disp,2),recursive=FALSE)
  # r3 <- r3[sapply(r3,length)==min(sapply(r3,length))]
  # cat(i,"- 3:",length(r3),"\n")
  # s[i] <- as.integer(substr(input[i],1,3))*min(sapply(r3,length))
  # cat(r3[which.min(sapply(r3,length))][[1]],"\n")
  cat(s[i],"\n")
}

cat(sum(s))
#197560

pairs <- list("<A"=c(">>^A"),"<<"=c("A"),"<v"=c(">A"),"<^"=c(">^A"),
     "^A"=c(">A"),"^^"=c("A"),"^<"=c("v<"),"^>"=c("v>",">v"),
     ">A"=c("^A"),">>"=c("A"),">^"=c("^<","<^"),">v"=c("<A"),
     "vA"=c(">^A","^>A"),"v<"=c("<A"),"v>"=c(">A"),"vv"=c("A"),
     "AA"=c("A"),"A<"=c("v<<A"),"A>"=c("vA"),"A^"=c("<A"),"Av"=c("<vA","v<A"))

find_disp2 <-\(x){
  A <- apply(embed(c("A",strsplit(x,"")[[1]]),2)[,2:1],1,paste,collapse="")
  p <- lapply(A,\(x)pairs[x][[1]])
  r <- ""
  for(i in seq_along(p)){
    if(length(r)==1) r <- paste0(r,p[[i]])
    if(length(r)>1){
      nr <- c()
      for(j in seq_along(r)) nr <- c(nr,paste0(r[j],p[[i]]))
      r <- nr
    }
  }
  r
}

s <- c()
for(i in 1:5){
  r1 <- find_disp(inp[[i]],1)
  r1 <- sapply(r1,paste,collapse="")
  r2 <- r1
  for(j in 1:25){
    r2 <- unlist(sapply(r2,find_disp2))
    n <- sapply(r2,nchar)
    r2 <- r2[n==min(n)]
    cat(min(n),"\n")
  }
  s[i] <- as.integer(substr(input[i],1,3))*nchar(r2[1])
}
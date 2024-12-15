input <- read.input(15)
input <- readLines(input)
w <- which(input=="")
map <- do.call(rbind,strsplit(input[1:(w[1]-1)],""))
inst <- unlist(strsplit(input[(w[1]+1):(w[2]-1)],""))
movebox <- \(o,i,x){
  n <- o+i
  NPO <- map[n[1],n[2]]
  if(NPO=="#") return(x)
  if(NPO=="."){
    map[n[1],n[2]] <<- "O"
    map[(x+i)[1],(x+i)[2]] <<- "."
    return(x+i)
  }
  if(NPO=="O") movebox(n,i,x)
}
move <- \(x,m){
  npos <- x+m
  NP <- map[npos[1],npos[2]]
  if(NP=="#") return(x)
  if(NP==".") return(npos)
  if(NP=="O") movebox(npos,m,x)
}
for(i in seq_along(inst)){
  pos <- which(map=="@",arr.ind=TRUE)
  mode <- switch(inst[i],"^"=1,">"=2,"v"=3,"<"=4)
  dir <- rbind(c(-1,0),c(0,1),c(1,0),c(0,-1))
  npos <- move(pos,dir[mode,])
  map[pos[1],pos[2]] <- "."
  map[npos[1],npos[2]] <- "@"
  cat(i,"\r")
}
w <- which(map=="O",arr.ind=TRUE)
w <- w -1
sum(100*w[,1]+w[,2])
#1538871

input2 <- gsub("#","##",input)
input2 <- gsub("O","[]",input2)
input2 <- gsub("\\.","..",input2)
input2 <- gsub("@","@.",input2)
w <- which(input2=="")
map <- do.call(rbind,strsplit(input2[1:(w[1]-1)],""))

move2 <- \(x,m){
  npos <- x+m
  NP <- map[npos[1],npos[2]]
  if(NP=="#") return(x)
  if(NP==".") return(npos)
  if(NP%in%c("[","]")){
    if(NP=="["){
      if(m[1]==0) mb <- movebox_horiz(rbind(npos,npos+c(0,1)),m,x,c("[","]"))
      if(m[2]==0) mb <- movebox_vert(rbind(npos,npos+c(0,1)),m,x,
                                     data.frame(x=c(npos[1],npos[1]),
                                                y=c(npos[2],npos[2]+1),
                                                b=c("[","]")))
    }
    if(NP=="]"){
      if(m[1]==0) mb <- movebox_horiz(rbind(npos+c(0,-1),npos),m,x,c("[","]"))
      if(m[2]==0) mb <- movebox_vert(rbind(npos+c(0,-1),npos),m,x,
                                     data.frame(x=c(npos[1],npos[1]),
                                                y=c(npos[2]-1,npos[2]),
                                                b=c("[","]")))
    }
    return(mb)
  }
}

movebox_horiz <- \(o,i,x,b){
  if(i[2]==1){
    n <- o[2,]+i
  }else{
    n <- o[1,]+i
  }
  NPO <- map[n[1],n[2]]
  if(NPO=="#") return(x)
  if(NPO=="."){
    range <- n[2]:x[2]
    map[n[1],range] <<- "."
    if(n[2]<x[2]) map[n[1],head(range,-2)] <<- b
    if(n[2]>x[2]) map[n[1],head(range,-2)] <<- rev(b)
    return(x+i)
  }
  if(NPO=="[")return(movebox_horiz(rbind(n,n+c(0,1)),i,x,c(b,"[","]")))
  if(NPO=="]")return(movebox_horiz(rbind(n+c(0,-1),n),i,x,c(b,"[","]")))
}

movebox_vert <- \(o,i,x,df){
  n <- o
  for(j in 1:nrow(n)){
    n[j,] <- n[j,]+i
  }
  npo <- c()
  for(j in 1:nrow(n)){
    npo[j] <- map[n[j,1],n[j,2]]
  }
  if(any(npo=="#")) return(x)
  if(all(npo==".")){
    for(j in 1:nrow(df)){
      map[df$x[j],df$y[j]] <<- "."
    }
    for(j in 1:nrow(df)){
      map[df$x[j]+i[1],df$y[j]] <<- df$b[j]
    }
    return(x+i)
  }else{
    for(j in seq_along(npo)){
      if(npo[j]=="]" & !any(n[,1]==n[j,1]&n[,2]==n[j,2]-1)){
        n <- rbind(n,c(n[j,1],n[j,2]-1))
        npo <- c(npo,"[")
      }
      if(npo[j]=="[" & !any(n[,1]==n[j,1]&n[,2]==n[j,2]+1)){
        n <- rbind(n,c(n[j,1],n[j,2]+1))
        npo <- c(npo,"]")
      }
    }
    df2 <- data.frame(x=n[npo!=".",1],y=n[npo!=".",2],b=npo[npo!="."])
    df <- rbind(df,df2)
    return(movebox_vert(n[npo%in%c("[","]"),],i,x,df))
  }
  
}

for(i in seq_along(inst)){
  pos <- which(map=="@",arr.ind=TRUE)
  mode <- switch(inst[i],"^"=1,">"=2,"v"=3,"<"=4)
  dir <- rbind(c(-1,0),c(0,1),c(1,0),c(0,-1))
  npos <- move2(pos,dir[mode,])
  map[pos[1],pos[2]] <- "."
  map[npos[1],npos[2]] <- "@"
  mm <- apply(map,1,paste,collapse="")
  if(any(grepl("]]",mm))|
     any(grepl("\\[\\[",mm))|
     any(grepl("\\.]",mm))|
     any(grepl("\\[\\.",mm)))break
  cat(i,"\r")
  # for(j in 1:nrow(map)){
  #   cat(map[j,],"\n",sep="")
  # }
}
w <- which(map=="[",arr.ind=TRUE)
w <- w -1
sum(100*w[,1]+w[,2])
#1543338
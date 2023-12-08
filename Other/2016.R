#Day 1
## Part 1
input <- el(strsplit(readLines(read.input(2016,1)),", "))
loc <- c(0,0)
dir <- 90
for(i in seq_along(input)){
  d <- substr(input[i],1,1)
  l <- as.integer(substr(input[i],2,nchar(input[i])))
  if(d=="R") dir <- dir - 90
  if(d=="L") dir <- dir + 90
  if(dir>=360) dir <- dir - 360
  if(dir<0) dir <- dir + 360
  if(dir==0) loc <- loc + l*c(1,0)
  if(dir==90) loc <- loc + l*c(0,1)
  if(dir==180) loc <- loc + l*c(-1,0)
  if(dir==270) loc <- loc + l*c(0,-1)
}
sum(abs(loc))
#243

## Part 2
loc <- c(0,0)
all_loc <- loc
dir <- 90
for(i in seq_along(input)){
  d <- substr(input[i],1,1)
  l <- as.integer(substr(input[i],2,nchar(input[i])))
  if(d=="R") dir <- dir - 90
  if(d=="L") dir <- dir + 90
  if(dir>=360) dir <- dir - 360
  if(dir<0) dir <- dir + 360
  if(dir==0) inc <- c(1,0)
  if(dir==90) inc <- c(0,1)
  if(dir==180) inc <- c(-1,0)
  if(dir==270) inc <- c(0,-1)
  for(j in 1:l){
    loc <- loc + inc
    all_loc <- rbind(all_loc,loc)
  }
}
sum(abs(all_loc[duplicated(all_loc),][1,]))
#142

# Day 2
## Part 1
pad <- matrix(1:9,ncol=3,byrow=TRUE)
loc <- c(2,2)
input <- el(strsplit(paste(scan(read.input(2016,2),""),collapse="\n"),""))
res <- c()
for(i in seq_along(input)){
  a <- input[i]
  if(a=="U"){
    loc <- loc + c(-1,0)
    loc[loc==0]<-1
  }
  if(a=="D"){
    loc <- loc + c(1,0)
    loc[loc==4]<-3  
  }
  if(a=="L"){
    loc <- loc + c(0,-1)
    loc[loc==0]<-1
  }
  if(a=="R"){
    loc <- loc + c(0,1)
    loc[loc==4]<-3  
  }
  if(a=="\n"){
    res <- c(res,pad[loc[1],loc[2]])
  }
}
res <- c(res,pad[loc[1],loc[2]])
cat(res,sep="")
#65556

## Part 2
pad <- rbind(c("","",1,"",""),c("",2:4,""),5:9,c("","A","B","C",""),c("","","D","",""))
loc <- c(2,2)
res <- c()
for(i in seq_along(input)){
  a <- input[i]
  if(a=="\n"){
    res <- c(res,pad[loc[1],loc[2]])
  }else{
    if(a=="U") inc <- c(-1,0)
    if(a=="D") inc <- c(1,0)
    if(a=="L") inc <- c(0,-1)
    if(a=="R") inc <- c(0,1)
    n <- loc + inc
    if(!any(n%in%c(0,6))) if(pad[n[1],n[2]]!="") loc <- loc + inc
  }
}
res <- c(res,pad[loc[1],loc[2]])
cat(res,sep="")
#CB779

# Day 3
## Part 1
input <- read.fwf(read.input(2016,3),width=c(5,5,5))
sum(apply(input,1,\(x) x[1]+x[2]>x[3] & 
                       x[1]+x[3]>x[2] & 
                       x[2]+x[3]>x[1]), na.rm=T)
#1032

## Part 2
input <- input[!is.na(input[,1]),]
new_input <- matrix(c(input[,1],input[,2],input[,3]),ncol=3,byrow=TRUE)
sum(apply(new_input,1,\(x) x[1]+x[2]>x[3] & 
            x[1]+x[3]>x[2] & 
            x[2]+x[3]>x[1]), na.rm=T)
#1838

# Day 4
## Part 1
input <- readLines(read.input(2016,4))
check <- substr(unlist(regmatches(input,gregexpr("\\[([a-z]+)\\]$",input))),2,6)
spl <- strsplit(gsub("-","",unlist(regmatches(input,gregexpr("^[^0-9]+",input)))),"")
s <- sapply(spl,\(x){
  s <- sort(table(x),decreasing=TRUE)
  paste(names(s)[1:5],collapse="")
  })
id <- as.integer(unlist(regmatches(input,gregexpr("[0-9]+",input))))
sum(id[s==check])
#185371

## Part 2
spl <- strsplit(unlist(regmatches(input,gregexpr("^[^0-9]+",input))),"")
good <- spl[s==check]
ids <- id[s==check]
decrypted <- c()
for(i in seq_along(good)){
  p <- good[[i]]
  for(j in seq_along(p)){
    if(p[j]!="-"){
      w <- which(letters==p[j]) + ids[i]
      if(w>26) w <- (w%%26)
      if(w==0) w <- 26
      p[j] <- letters[w]
    }
  }
  decrypted[i] <- paste(p,collapse="")
}
ids[grep("northpole-object-storage",decrypted)]
#984

# Day 5
## Part 1
library(openssl)
input <- "uqwqemis"
#input <- "abc"
i <- 0
res <- c()
while(length(res)<8){
  m <- paste0(input,i)
  md <- digest::digest(m,"md5",serialize=FALSE)
  if(substr(md,1,5)=="00000"){
    res <- c(res,substr(md,6,6))
  }
  i <- i +1
  cat(res,"\r")
}


# Day 6
## Part 1
input <- readLines(read.input(2016,6))
input <- do.call(rbind,strsplit(input,""))
cat(apply(input,2,\(x)names(sort(table(x),decreasing=TRUE))[1]),sep="")
#cyxeoccr

## Part 2
cat(apply(input,2,\(x)names(sort(table(x),decreasing=FALSE))[1]),sep="")
#batwpask

# Day 7
## Part 1
input <- readLines(read.input(2016,7))
step1 <- strsplit(input,"\\[[^][]+\\]")
step2 <- sapply(step1,\(x)any(grepl("(.)((?!\\1).)\\2\\1",x,perl=TRUE)))
step3 <- regmatches(input,gregexpr("\\[[^][]+\\]",input))
step4 <- sapply(step3,\(x)any(grepl("(.)((?!\\1).)\\2\\1",x,perl=TRUE)))
sum(step2 & !step4)
#115

# Day 8
## Part 1
input <- readLines(read.input(2016,8))
screen <- matrix(FALSE,ncol=50,nrow=6)
for(i in seq_along(input)){
  nb <- as.integer(regmatches(input[i],gregexpr("[0-9]+",input[i]))[[1]])
  if(grepl("rect",input[i])){
    screen[1:nb[2],1:nb[1]] <- TRUE
  }
  if(grepl("rotate row",input[i])){
    o <- screen[nb[1]+1,]
    if(nb[2]>=ncol(screen)) nb[2] <- nb[2]%%ncol(screen)
    if(nb[2]) screen[nb[1]+1,] <- c(o[(ncol(screen)-nb[2]+1):ncol(screen)],o[1:(ncol(screen)-nb[2])])
  }
  if(grepl("rotate column",input[i])){
    o <- screen[,nb[1]+1]
    if(nb[2]>=nrow(screen)) nb[2] <- nb[2]%%nrow(screen)
    if(nb[2]) screen[,nb[1]+1] <- c(o[(nrow(screen)-nb[2]+1):nrow(screen)],o[1:(nrow(screen)-nb[2])])
  }
}
sum(screen)
#110

## Part 2
par(mar=c(0,0,0,0));image(1:50,1:6,t(screen),ylim=c(6,1))
#ZJHRKCPLYJ

# Day 9
## Part 1
input <- readLines(read.input(2016,9))[1]
output <- ""
i <- 1
while(i<=nchar(input)){
  s <- substr(input,i,i)
  if(s %in% LETTERS){
    output <- paste0(output,s)
    i <- i+1
  }else{
    g <- gsub("^\\(([0-9]+x[0-9]+)\\).*$","\\1",substr(input,i,nchar(input)))
    n <- as.integer(strsplit(g,"x")[[1]])
    s <- paste(rep(substr(input,i+nchar(g)+2,i+nchar(g)+1+n[1]),n[2]),collapse="")
    output <- paste0(output,s)
    i <- i + nchar(g)+2+n[1]
    }
}
nchar(output)
#70186

## Part 2
i <- 1
w <- rep(1,nchar(input))
l <- 0
while(i<=nchar(input)){
  s <- substr(input,i,i)
  if(s %in% LETTERS){
    l <- l+w[i]
    i <- i+1
  }else{
    g <- gsub("^\\(([0-9]+x[0-9]+)\\).*$","\\1",substr(input,i,nchar(input)))
    n <- as.integer(strsplit(g,"x")[[1]])
    w[(i+nchar(g)+2):(i+nchar(g)+1+n[1])] <- w[(i+nchar(g)+2):(i+nchar(g)+1+n[1])] * n[2]
    i <- i + nchar(g)+2
    }
  cat(i,"\r")
}
l
#10915059201

# Day 10
## Part 1
input <- readLines(read.input(2016,10))
bots <- outputs <- list()
value <- input[grepl("^value",input)]
comp <- input[grepl("^bot",input)]
init <- parse.group("value (?<v>[0-9]+) goes to bot (?<n>[0-9]+)",value)
comps <- parse.group("bot (?<b>[0-9]+) gives low to (?<lo>[a-z]+) (?<l>[0-9]+) and high to (?<ho>[a-z]+) (?<h>[0-9]+)",comp)
init_bot <- init$n[duplicated(init$n)]
for(i in 1:nrow(init)){
  bots[[init$n[i]]] <- as.integer(init$v[init$n==init$n[i]])
}
while(nrow(comps)){
  if(any(sapply(bots,length)==2)){
    b <- names(bots[sapply(bots,length)==2])
    for(i in seq_along(b)){
      if(61%in%bots[[b[i]]] & 17%in%bots[[b[i]]]) cat("Values 61 and 17 in bot ",b[i],"\n")
      co <- comps[comps$b==b[i],]
      comps <- comps[comps$b!=b[i],]
      bl <- co$l
      bh <- co$h
      if(co$lo=="bot"){
        if(bl%in%names(bots)){
          bots[[bl]] <- c(bots[[bl]], min(bots[[b[i]]]))
        }else{
          bots[[bl]] <- min(bots[[b[i]]])
        }
      }else{
        outputs[[bl]] <- min(bots[[b[i]]])
      }
      if(co$ho=="bot"){
        if(bh%in%names(bots)){
          bots[[bh]] <- c(bots[[bh]], max(bots[[b[i]]]))
        }else{
          bots[[bh]] <- max(bots[[b[i]]])
        } 
      }else{
        outputs[[bh]] <- max(bots[[b[i]]])
      }
      bots[[b[i]]] <- NULL
    }
  }else{
    break
  }
}
#47

## Part 2
outputs$`0`*outputs$`1`*outputs$`2`
#2666

# Day 12
## Part 1
reg <- list(a=0,b=0,c=0,d=0)
input <- readLines(read.input(2016,12))
i <- 1
repeat{
  inp <- input[i]
  cat(inp,reg$a,reg$b,reg$c,reg$d,"\n",sep="\t",file="test.txt",append=TRUE)
  if(grepl("^cpy",inp)){
    s <- strsplit(inp," ")[[1]]
    if(!is.na(as.integer(s[2]))){
      reg[[s[3]]] <- as.integer(s[2])
    }else{
      reg[[s[3]]] <- reg[[s[2]]]
    }
    i <- i + 1
  }
  if(grepl("^inc",inp)){
    s <- strsplit(inp," ")[[1]]
    reg[[s[2]]] <- reg[[s[2]]] + 1
    i <- i + 1
  }
  if(grepl("^dec",inp)){
    s <- strsplit(inp," ")[[1]]
    reg[[s[2]]] <- reg[[s[2]]] - 1
    i <- i + 1
  }
  if(grepl("^jnz",inp)){
    s <- strsplit(inp," ")[[1]]
    if(!is.na(as.integer(s[2]))){
      if(as.integer(s[2])){
        i <- i + as.integer(s[3])
      }else{
        i <- i + 1
      }
    }else{
      if(reg[[s[2]]]){
        i <- i + as.integer(s[3])
      }else{
        i <- i+1
      }
    }
  }
  if(i>=length(input)) break
}
reg$a
#318020

## Part 2
reg <- list(a=0,b=0,c=1,d=0)
input <- readLines(read.input(2016,12))
i <- 1
repeat{
  inp <- input[i]
  cat(inp,reg$a,reg$b,reg$c,reg$d,"\n",sep="\t",file="test.txt",append=TRUE)
  if(grepl("^cpy",inp)){
    s <- strsplit(inp," ")[[1]]
    if(!is.na(as.integer(s[2]))){
      reg[[s[3]]] <- as.integer(s[2])
    }else{
      reg[[s[3]]] <- reg[[s[2]]]
    }
    i <- i + 1
  }
  if(grepl("^inc",inp)){
    s <- strsplit(inp," ")[[1]]
    reg[[s[2]]] <- reg[[s[2]]] + 1
    i <- i + 1
  }
  if(grepl("^dec",inp)){
    s <- strsplit(inp," ")[[1]]
    reg[[s[2]]] <- reg[[s[2]]] - 1
    i <- i + 1
  }
  if(grepl("^jnz",inp)){
    s <- strsplit(inp," ")[[1]]
    if(!is.na(as.integer(s[2]))){
      if(as.integer(s[2])){
        i <- i + as.integer(s[3])
      }else{
        i <- i + 1
      }
    }else{
      if(reg[[s[2]]]){
        i <- i + as.integer(s[3])
      }else{
        i <- i+1
      }
    }
  }
  if(i>=length(input)) break
}
reg$a
#9227674

# Day 13 (Unfinished)
## Part 1
f <- function(x,y) x^2+3*x+2*x*y+y+y^2+1352
wo <- function(x,y){
  n <- f(x,y)
  !sum(intToBits(n)==1)%%2
}
map <- matrix(nr=50,nc=50)
for(i in 1:50){
  for(j in 1:50){
    map[i,j] <- wo(i,j)
  }
}
w <- which(map,arr.ind=T)
W <- 1:nrow(w)
edges <- data.frame(from=c(),to=c())
for(i in seq_along(W)){
  a <- w[i,]
  if(any(w[,1]==a[1]+1&w[,2]==a[2])){
    b <- W[w[,1]==a[1]+1&w[,2]==a[2]]
    edges <- rbind(edges,data.frame(from=W[i],to=b))
  }
  if(any(w[,1]==a[1]-1&w[,2]==a[2])){
    b <- W[w[,1]==a[1]-1&w[,2]==a[2]]
    edges <- rbind(edges,data.frame(from=W[i],to=b))
  }
  if(any(w[,1]==a[1]&w[,2]==a[2]+1)){
    b <- W[w[,1]==a[1]&w[,2]==a[2]+1]
    edges <- rbind(edges,data.frame(from=W[i],to=b))
  }
  if(any(w[,1]==a[1]&w[,2]==a[2]-1)){
    b <- W[w[,1]==a[1]&w[,2]==a[2]-1]
    edges <- rbind(edges,data.frame(from=W[i],to=b))
  }
}
W1 <- W[w[,1]==1&w[,2]==1]
W2 <- W[w[,1]==31&w[,2]==39]
edges <- apply(as.matrix(edges),2,as.character)
library(igraph)
g <- graph_from_data_frame(edges,directed=FALSE)
distances(g,as.character(W1),as.character(W2))
#90

## Part 2

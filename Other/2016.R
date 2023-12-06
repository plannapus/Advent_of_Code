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


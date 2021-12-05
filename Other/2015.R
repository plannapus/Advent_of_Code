# Day 1
## Part 1
diff(rev(table(el(strsplit(scan("input/2015_input01.txt",""),"")))))
# 138

## Part 2
input <- el(strsplit(scan("input/2015_input01.txt",""),""))
input[input=="("] <- 1
input[input==")"] <- -1
cs <- cumsum(as.integer(input))
head(which(cs<0),1)
# 1771

# Day 2
## Part 1
tab <- read.table("input/2015_input02.txt",sep="x")
small <- apply(tab,1,function(x)x[which.min(x)])
areas <- t(apply(tab,1,function(x)c(2*x[1]*x[2],2*x[1]*x[3],2*x[2]*x[3])))
sum(apply(areas,1,function(x)sum(x)+x[which.min(x)]/2))
# 1606483

## Part 2
sum(apply(tab,1,function(x)prod(x)+2*sum(x[-which.max(x)])))
# 3842356

# Day 3
## Part 1
input <- el(strsplit(scan("input/2015_input03.txt",""),""))
visited <- matrix(c(0,0),nrow=1)
for(i in seq_along(input))  visited <- rbind(visited, visited[nrow(visited),]+switch(input[i],"^"=c(0,1),">"=c(1,0),"<"=c(-1,0),"v"=c(0,-1)))
visited <- visited[!duplicated(visited),]
nrow(visited)
# 2572

## Part 2
santa <- robosanta <- matrix(c(0,0),nrow=1)
for(i in seq_along(input)){
  if(i%%2){
    santa <- rbind(santa, santa[nrow(santa),]+switch(input[i],"^"=c(0,1),">"=c(1,0),"<"=c(-1,0),"v"=c(0,-1)))
  }else{
    robosanta <- rbind(robosanta, robosanta[nrow(robosanta),]+switch(input[i],"^"=c(0,1),">"=c(1,0),"<"=c(-1,0),"v"=c(0,-1)))
  }
}
visited <- rbind(santa, robosanta)
visited <- visited[!duplicated(visited),]
nrow(visited)
# 2631

# Day 4
## Part 1
library(openssl)
input <- "yzbqklnj"
i <- 1
while(!grepl("^0{5}",md5(paste0(input,i)))) i <- i+1
i
# 282749

## Part 2
while(!grepl("^0{6}",md5(paste0(input,i)))){
  i <- i+1
  if(!i%%10000) cat(i,"\r")
}
i
#9962624

# Day 5
## Part 1
input <- scan("input/2015_input05.txt","")
is_nice <- function(x)  !any(sapply(c("ab","cd","pq","xy"),grepl,x)) & 
                        grepl("[aeiou].*[aeiou].*[aeiou].*",x) & 
                        grepl("(.)\\1",x)
sum(sapply(input,is_nice))
# 255

## Part 2
is_nice2 <- function(x) grepl("(..).*\\1",x,perl=TRUE) & grepl("(.).\\1",x)
sum(sapply(input,is_nice2))
# 55

# Day 6
## Part 1
input <- readLines("input/2015_input06.txt")
map <- matrix(FALSE,nr=1000,nc=1000)
coords <- 1+do.call(rbind,lapply(regmatches(input,gregexpr("[0-9]+",input)),as.integer))
action <- gsub("^([^0-9]+) [0-9].+$","\\1",input)
for(i in seq_along(input)){
  if(action[i]=="toggle"){  
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- !map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]]
  }else if(action[i]=="turn on"){
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- TRUE
  }else if(action[i]=="turn off"){
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- FALSE
  }
}
sum(map)
# 377891

## Part 2
map <- matrix(0,nr=1000,nc=1000)
for(i in seq_along(input)){
  if(action[i]=="toggle"){  
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]]+2
  }else if(action[i]=="turn on"){
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]]+1
  }else if(action[i]=="turn off"){
    map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]] <- map[coords[i,1]:coords[i,3],coords[i,2]:coords[i,4]]-1
  }
  map[map<0] <- 0
}
sum(map)
# 14110788

# Day 7 (in progress)
## Part 1
input <- readLines("input/2015_input07.txt")
tab <- do.call(rbind,strsplit(input," -> "))
variables <- rep(NA,nrow(tab))
names(variables) <- tab[,2]
variables[which(grepl("^[0-9 ]+$",tab[,1]))]<-as.integer(tab[which(grepl("^[0-9 ]+$",tab[,1])),1])

bitify <- function(x){
  res <- c()
  for(i in 15:0){
    res <- c(res,x%/%(2^i))
    x <- x%%(2^i)
  }
  res
}

unbitify <- function(x) sum(x*2^(15:0)) 

lshift <- function(x,n){
  b <- bitify(x)
  b <- c(b[-(1:n)],b[1:n])
  unbitify(b)
}
rshift <- function(x,n){
  b <- bitify(x)
  b <- c(tail(b,n),head(b,-n))
  unbitify(b)
}
And <- function(x,y){
  b1 <- bitify(x)
  b2 <- bitify(x)
  unbitify(b1*b2)
}
Or <- function(x,y){
  b1 <- bitify(x)
  b2 <- bitify(x)
  unbitify(b1+b2)
}
Not <- function(x){
  b <- bitify(x)
  unbitify(as.integer(!b))
}

eq <- list()
for(i in seq_along(tab[,1])){
  eq[[i]]<-list()
  eq[[i]]$res <- tab[i,2]
  if(grepl("NOT ",tab[i,1])){
    eq[[i]]$type <- "NOT"
    eq[[i]]$var <- gsub("NOT ","",tab[i,1])
  }else if(grepl(" OR ",tab[i,1])){
    eq[[i]]$type <- "OR"
    eq[[i]]$var1 <- gsub("^([^ ]+) OR.+$","\\1",tab[i,1])
    eq[[i]]$var2 <- gsub("^.+ OR ([^ ]+)$","\\1",tab[i,1])
    if(!eq[[i]]$var1%in%names(variables)) eq[[i]]$var1 <- as.integer(eq[[i]]$var1)
    if(!eq[[i]]$var2%in%names(variables)) eq[[i]]$var2 <- as.integer(eq[[i]]$var2)
  }else if(grepl(" AND",tab[i,1])){
    eq[[i]]$type <- "AND"
    eq[[i]]$var1 <- gsub("^([^ ]+) AND.+$","\\1",tab[i,1])
    eq[[i]]$var2 <- gsub("^.+ AND ([^ ]+)$","\\1",tab[i,1])
    if(!eq[[i]]$var1%in%names(variables)) eq[[i]]$var1 <- as.integer(eq[[i]]$var1)
    if(!eq[[i]]$var2%in%names(variables)) eq[[i]]$var2 <- as.integer(eq[[i]]$var2)
  }else if(grepl(" RSHIFT ",tab[i,1])){
    eq[[i]]$type <- "RSHIFT"
    eq[[i]]$var <- gsub("^([^ ]+) RSHIFT.+$","\\1",tab[i,1])
    eq[[i]]$n <- as.integer(gsub("^.+ RSHIFT ([^ ]+)$","\\1",tab[i,1]))
  }else if(grepl(" LSHIFT ",tab[i,1])){
    eq[[i]]$type <- "LSHIFT"
    eq[[i]]$var <- gsub("^([^ ]+) LSHIFT.+$","\\1",tab[i,1])
    eq[[i]]$n <- as.integer(gsub("^.+ LSHIFT ([^ ]+)$","\\1",tab[i,1]))
  }else{
    eq[[i]]$type <- "NONE"
    eq[[i]]$var <- tab[i,1]
  }
}


while(any(is.na(variables))){
  subs <- variables[is.na(variables)]
  remaining_eq <- eq[tab[,2]%in%names(subs)]
  solved <- variables[!is.na(variables)]
  for(i in seq_along(remaining_eq)){
    if(remaining_eq[[i]]$type=="NOT"){
      if(remaining_eq[[i]]$var%in%names(solved)){
        variables[remaining_eq[[i]]$res] <- Not(solved[remaining_eq[[i]]$var])
      }
    }
    if(remaining_eq[[i]]$type=="OR"){
      if(remaining_eq[[i]]$var2%in%names(solved)&(remaining_eq[[i]]$var1%in%names(solved)|is.integer(remaining_eq[[i]]$var1))){
        y <- ifelse(is.integer(remaining_eq[[i]]$var1),remaining_eq[[i]]$var1,solved[remaining_eq[[i]]$var1])
        variables[remaining_eq[[i]]$res] <- Or(solved[remaining_eq[[i]]$var1],y)
      }
    }
    if(remaining_eq[[i]]$type=="AND"){
      if(remaining_eq[[i]]$var2%in%names(solved)&(remaining_eq[[i]]$var1%in%names(solved)|is.integer(remaining_eq[[i]]$var1))){
        y <- ifelse(is.integer(remaining_eq[[i]]$var1),remaining_eq[[i]]$var1,solved[remaining_eq[[i]]$var1])
        variables[remaining_eq[[i]]$res] <- And(solved[remaining_eq[[i]]$var1],y)
      }
    }
    if(remaining_eq[[i]]$type=="RSHIFT"){
      if(remaining_eq[[i]]$var%in%names(solved)){
        variables[remaining_eq[[i]]$res] <- rshift(solved[remaining_eq[[i]]$var],remaining_eq[[i]]$n)
      }
    }
    if(remaining_eq[[i]]$type=="LSHIFT"){
      if(remaining_eq[[i]]$var%in%names(solved)){
        variables[remaining_eq[[i]]$res] <- lshift(solved[remaining_eq[[i]]$var],remaining_eq[[i]]$n)
      }
    }
    if(remaining_eq[[i]]$type=="NONE"){
      if(remaining_eq[[i]]$var%in%names(solved)){
        variables[remaining_eq[[i]]$res] <- solved[remaining_eq[[i]]$var]
      }
    }
  }
}

variables['a']


#Day 8
## Part 1
input <- readLines("input/2015_input08.txt")
input2 <- scan("input/2015_input08.txt","")
cat(input2,sep="",file="input/2015_flush08.txt")
input3 <- scan("input/2015_flush08.txt","",allowEscapes=TRUE)
cat(input3,sep="\n",file="input/2015_flush08.txt")
s2 <- as.integer(gsub("^ +([0-9]+) .+$","\\1",system("wc -c input/2015_flush08.txt",intern=TRUE)))
sum(nchar(input))-s2+length(input3)-1 #Ugly but seems to work
# 1350

## Part 2
u <- table(unlist(strsplit(input,"")))
2*length(input)+u['\\']+u['\"']
#2085

#Day 9
## Part 1
input <- readLines("input/2015_input09.txt")
df <- as.data.frame(do.call(rbind,strsplit(input,"( to )|( = )")))
df$V3 <- as.integer(df$V3)
towns <- unique(c(df$V1,df$V2))
library(combinat)
pt <- permn(towns)
d <- rep(0,length(pt))
for(i in seq_along(pt)){
  for(j in 1:7){
    d[i] <- d[i] + c(df[df$V1==pt[[i]][j]&df$V2==pt[[i]][j+1],3],df[df$V2==pt[[i]][j]&df$V1==pt[[i]][j+1],3])
  }
  cat(i,"\r")
}
min(d)
#251

## Part 2
max(d)
#898

# Day 10
## Part 1
input <- "1113222113"
for(i in 1:40){
  rl <- rle(el(strsplit(input,"")))
  input <- paste0(paste0(rl$l,rl$v),collapse="")
}
nchar(input)
#252594

input <- "1113222113"
for(i in 1:50){
  rl <- rle(el(strsplit(input,"")))
  input <- paste0(paste0(rl$l,rl$v),collapse="")
}
nchar(input)
#3579328

# Day 11
## Part 1 (Does not work!)
input <- "hepxcrrq"
password <- el(strsplit(input,""))
s <- sapply(password,function(x)which(letters==x))
cond1 <- function(s){
  r <- rle(diff(s))
  any(r$l[r$v==1]>1)
}
cond2 <- function(s){
  !any(s%in%c(9,12,15))
}
cond3 <- function(s){
  r <- rle(apply(embed(s,2),1,function(x)x[1]==x[2]))
  if(any(r$v)){
    return(any(r$l[r$v]>2) | length(r$l[r$v])>1)
  }else{
    return(FALSE)
  }
}

while(!(cond1(s)&cond2(s)&cond3(s))){
  testset <- cbind(s[1],s[2],s[3],s[4],s[5],expand.grid(1:26,1:26,1:26))
  A <- apply(testset,1,function(x)cond1(x)&cond2(x)&cond3(x))
  if(any(A)){
    cat(letters[c(t(testset[A,]))],sep="")
    break
  }else{
    s[5]<-s[5]+1
    if(s[5]%in%c(9,12,15))s[5] <- s[5]+1
    if(s[5]>26){
      s[4] <- s[4]+1
      s[5] <- 1
      if(s[4]%in%c(9,12,15))s[4] <- s[4]+1
      if(s[4]>26){
        s[3] <- s[3]+1
        s[4] <- 1
        if(s[3]%in%c(9,12,15))s[3] <- s[3]+1
        if(s[3]>26){
          s[2] <- s[2]+1
          s[3] <- 1
          if(s[2]%in%c(9,12,15))s[2] <- s[2]+1
          if(s[2]>26){
            s[1] <- s[1]+1
            s[2] <- 1
            if(s[1]%in%c(9,12,15))s[1] <- s[1]+1
          }
        }
      }
    }
  }
}
#heqxxyzz
s <- c(t(testset[A,]))+c(rep(0,4),1,0,0,0)
#heqaabcc

# Day 12
## Part 1
library(rjson)
input <- fromJSON(readLines("input/2015_input12.txt"))
sum(as.integer(unlist(input)),na.rm=T)
#119433

## Part 2
depthwise <- function(js){
  if("red"%in%js&!is.null(names(js))){
    js <- NA
  }
  if(is.list(js)){js <- sapply(js,depthwise)}
  js
}
output <- depthwise(input)
sum(as.integer(unlist(output)),na.rm=TRUE)
#68466

# Day 13
## Part 1
input <- readLines("input/2015_input13.txt")
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parsed <- regexpr("^(?<n1>[A-Z][a-z]+) .+ (?<sign>gain|lose) (?<score>[0-9]+) .+ (?<n2>[A-Z][a-z]+).$", input, perl=TRUE)
tab <- as.data.frame(parse.one(input,parsed))
tab$score <- as.integer(tab$score)
tab$score <- ifelse(tab$sign=="lose",-1*tab$score,tab$score)
library(combinat)
pu <- permn(unique(tab$n1))
hap <- rep(0,length(pu))
for(i in seq_along(pu)){
  sitting_plan <- embed(c(pu[[i]],pu[[i]][1]),2)
  hap[i] <-sum(apply(sitting_plan,1,function(x)tab$score[tab$n1==x[1]&tab$n2==x[2]]+tab$score[tab$n2==x[1]&tab$n1==x[2]]))
  cat(i,"\r")
}
max(hap)
#733

## Part 2
tab2 <- rbind(tab, data.frame(n1="me",n2=unique(tab$n1),sign="gain",score=0),
              data.frame(n2="me",n1=unique(tab$n1),sign="gain",score=0))
pu2 <- permn(unique(tab2$n1))
hap <- rep(0,length(pu2))
for(i in seq_along(pu2)){
  sitting_plan <- embed(c(pu2[[i]],pu2[[i]][1]),2)
  hap[i] <-sum(apply(sitting_plan,1,function(x)tab2$score[tab2$n1==x[1]&tab2$n2==x[2]]+tab2$score[tab2$n2==x[1]&tab2$n1==x[2]]))
  if(!as.logical(i%%1000))cat(i,"\r")
}
max(hap)
#725

#Day 14
## Part 1
input <- readLines("input/2015_input14.txt")
parsed <- regexpr("^(?<n1>[A-Z][a-z]+) .+ (?<speed>[0-9]+) km/s for (?<t1>[0-9]+) seconds, but then must rest for (?<t2>[0-9]+) seconds.$", input, perl=TRUE)
tab <- as.data.frame(parse.one(input,parsed))
tab$speed <- as.integer(tab$speed)
tab$t1 <- as.integer(tab$t1)
tab$t2 <- as.integer(tab$t2)
tab$dist <- 0

for(i in 1:nrow(tab)){
  distance <- 0
  time <- 0
  while(!any(time>=2503)){
    distance <- c(distance,tail(distance,1)+tab$speed[i]*(1:tab$t1[i]))
    time <- c(time,tail(time,1)+1:tab$t1[i])
    distance <- c(distance, tail(distance,1))
    time <- c(time, tail(time,1)+tab$t2[i])
  }
  w <- which(time>=2503)[1]
  tab$dist[i] <- distance[w]
}
max(tab$dist)
#2655

## Part 2
distance <- time <- list()
for(i in 1:nrow(tab)){
  distance[[i]] <- 0
  time[[i]] <- 0
  while(!any(time[[i]]>=2503)){
    distance[[i]] <- c(distance[[i]],tail(distance[[i]],1)+tab$speed[i]*(1:tab$t1[i]))
    time[[i]] <- c(time[[i]],tail(time[[i]],1)+1:tab$t1[i])
    distance[[i]] <- c(distance[[i]], tail(distance[[i]],1))
    time[[i]] <- c(time[[i]], tail(time[[i]],1)+tab$t2[i])
  }
}
pt <- rep(0,nrow(tab))
for(i in 1:2503){
  W <- which.max(sapply(1:nrow(tab),function(j)distance[[j]][which(time[[j]]>=i)[1]]))
  pt[W] <- pt[W]+1
}
max(pt)
#1059

# Day 15
## Part 1
input <- readLines("input/2015_input15.txt")
caract <- apply(do.call(rbind,regmatches(input,gregexpr("-?[0-9]+",input))),2,as.integer)
#eg <-expand.grid(0:100,0:100,0:100,0:100)
#eg <- eg[apply(eg,1,sum)==100,]
step1 <- do.call(rbind,lapply(0:100,function(x)expand.grid(x,0:(100-x))))
step2 <- do.call(rbind,lapply(1:nrow(step1),function(x)cbind(step1[x,],0:(100-sum(step1[x,])))))
colnames(step2)<-c("Sprinkles","Peanut","Frosting")
step2$Sugar<-100-rowSums(step2)
car <- cbind(apply(step2,1,function(x)sum(x*caract[,1])),
      apply(step2,1,function(x)sum(x*caract[,2])),
      apply(step2,1,function(x)sum(x*caract[,3])),
      apply(step2,1,function(x)sum(x*caract[,4])))
car[car<0]<-0
pr <- apply(car,1,prod)
max(pr)
# 13882464

## Part 2
calories <- apply(step2,1,function(x)sum(x*caract[,5]))
pr <- pr[calories==500]
max(pr)
# 11171160

# Day 16







# Day 20
input <- 29000000

houses <- rep(0,1e6)
for(i in 1:1e6){
  houses[seq(i,1e6,by=i)]<-houses[seq(i,1e6,by=i)]+i*10
  if(any(houses)>=input) break
  if(!i%%1000)cat(i,":",max(houses),"\r")
}
which(houses>=input)
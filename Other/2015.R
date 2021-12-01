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
input <- readLines("input19.txt")
w<-which(input=="")
messages <- input[(w+1):length(input)]
rules <- input[1:(w-1)]
rules <- as.data.frame(do.call(rbind,strsplit(rules,": ")))
rules[,2]<-paste0("(",gsub("\\\"","",rules[,2]),")")
rules[,1]<-as.integer(rules[,1])
rules <- rules[order(rules[,1]),]
#mod <- as.list(rules[,2])
W <- c()
while(grepl("[0-9]",rules[1,2])){
  w <- grep("^[^0-9]+$",rules[,2])
  w <- w[!w%in%W]
  for(i in seq_along(w)){
    ind <- rules[w[i],1]
    for(j in grep(sprintf("\\b%s\\b",ind),rules[,2])){
      rules[j,2] <- gsub(sprintf("\\b%s\\b",ind),rules[w[i],2],rules[j,2])
    }
  }
  W <- c(W,w) 
}
sum(grepl(sprintf("^%s$",gsub(" ","",rules[1,2])),messages))
#151

w<-which(input=="")
rules <- input[1:(w-1)]
rules <- as.data.frame(do.call(rbind,strsplit(rules,": ")))
rules[,2]<-paste0("(",gsub("\\\"","",rules[,2]),")")
rules[,1]<-as.integer(rules[,1])
rules <- rules[order(rules[,1]),]
rules[9,2] <- "(42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42 | 42 (42)*)*)*)*)*)*)*)*)*)*)*)"
rules[12,2] <- "(42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31 | 42 (42 31)* 31)* 31)* 31)* 31)* 31)* 31)* 31)* 31)* 31)* 31)* 31)"
W <- c()
while(grepl("[0-9]",rules[1,2])){
  w <- grep("^[^0-9]+$",rules[,2])
  w <- w[!w%in%W]
  for(i in seq_along(w)){
    ind <- rules[w[i],1]
    r <- rules[w[i],2]
    if(!grepl("\\|",r)) r<-gsub("^\\(|\\)","",r)
    for(j in grep(sprintf("\\b%s\\b",ind),rules[,2])){
      rules[j,2] <- gsub(sprintf("\\b%s\\b",ind),r,rules[j,2])
    }
  }
  W <- c(W,w) 
}
sum(grepl(sprintf("^%s$",gsub(" ","",rules[1,2])),messages))
#386
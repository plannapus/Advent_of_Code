input <- readLines("input07.txt")
i <- 2
pwd <- ""
tree <- list("/"=list())
while(i<length(input)){
  cat(input[i],"\n")
  if(grepl("^\\$ cd",input[i])){
    pwd <- paste(pwd,gsub("\\$ cd ","",input[i]),sep="/")
    if(grepl("\\.\\.$",pwd)) pwd <- gsub("/[^/]+/\\.\\.$","",pwd)
    cat(i,":",pwd)
    i <- i+1
  }
  if(input[i]=="$ ls"){
    j <- grep("^\\$",input[(i+1):length(input)])[1]
    if(is.na(j)){
        content <- input[(i+1):length(input)]
        i <- length(input)
      }else{
        content <- input[(i+1):(i+j-1)]
        i <- i+j
      }
    expr <- "list("
    for(k in seq_along(content)){
      if(grepl("^dir ",content[k])){
        name <- gsub("dir ","",content[k])
        expr <- paste0(expr,name,"=list(),")
      }else if(grepl("^[0-9]",content[k])){
        expr <- paste0(expr,gsub("[^0-9]","",content[k]),",")
      }
    }
    expr <- paste0(gsub(",$","",expr),")")
    d <- eval(parse(text=expr))
    path <- el(strsplit(pwd,"/"))
    path <- path[path!=""]
    expr<-"tree[[1]]"
    for(k in seq_along(path)){
        expr <- paste0(expr,"$",path[k])
    }
    expr <- paste0(expr,"<-d")
    eval(parse(text=expr))
    cat(i,":",pwd)
  }
}

foo <- function(x){
  s <- sum(unlist(x))
  w <- which(sapply(x,class)=="list")
  if(length(w)){
    S <- lapply(x[w],foo)
  }else{
    S <- 0
  }
  return(list(s,S))
}
allsizes <- foo(tree)
allsizes <- unlist(allsizes)
sum(allsizes[allsizes<100000])
#1444896

target<-30000000-(70000000-allsizes[1])
sort(allsizes[allsizes>=target])[1]
#404395

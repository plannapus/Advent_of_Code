input <- readLines("input07.txt")
i <- 2
pwd <- "" #our current path
tree <- list("/"=list()) #Our arborescence of directories
while(i<length(input)){
  if(grepl("^\\$ cd",input[i])){ #If cd change pwd and increment i by 1
    pwd <- paste(pwd,gsub("\\$ cd ","",input[i]),sep="/")
    if(grepl("\\.\\.$",pwd)) pwd <- gsub("/[^/]+/\\.\\.$","",pwd)
    i <- i+1
  }
  if(input[i]=="$ ls"){ #if ls grab content and increment by length of content
    j <- grep("^\\$",input[(i+1):length(input)])[1] #check where next command is
    if(is.na(j)){ #if we arrived at the end of the input
        content <- input[(i+1):length(input)]
        i <- length(input)
      }else{ #if not
        content <- input[(i+1):(i+j-1)]
        i <- i+j
      }
    expr <- "list(" #We'll use metaprogramming here
    for(k in seq_along(content)){
      if(grepl("^dir ",content[k])){
        name <- gsub("dir ","",content[k])
        expr <- paste0(expr,name,"=list(),") #if directory make a new sublist
      }else if(grepl("^[0-9]",content[k])){
        expr <- paste0(expr,gsub("[^0-9]","",content[k]),",") #if file just keep size of file
      }
    }
    expr <- paste0(gsub(",$","",expr),")")
    d <- eval(parse(text=expr)) #content as a sublist
    path <- el(strsplit(pwd,"/")) #now we figure out where to put it based on current path
    path <- path[path!=""]
    expr<-"tree[[1]]" #Our root
    for(k in seq_along(path)){
        expr <- paste0(expr,"$",path[k])
    }
    expr <- paste0(expr,"<-d")
    eval(parse(text=expr)) #Place sublist in correct list
  }
}

foo <- function(x){ #Recursive function to compute size of list and all its sublists
  s <- sum(unlist(x))
  w <- which(sapply(x,class)=="list")
  if(length(w)){
    S <- lapply(x[w],foo)
  }else{
    S <- 0 #if doesn't content more lists then 0
  }
  return(list(s,S))
}
allsizes <- foo(tree) #Sizes of all directories
allsizes <- unlist(allsizes)
sum(allsizes[allsizes<100000])
#1444896

target<-30000000-(70000000-allsizes[1]) #Extra space needed
sort(allsizes[allsizes>=target])[1]
#404395

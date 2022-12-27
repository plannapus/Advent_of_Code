options(digits=22)
input<-readLines("input21.txt")
input<-gsub(": "," <- ", input)
bytype<-split(input, grepl("[0-9]", input))
eval(parse(text=bytype[[2]]))
bl <- bytype[[1]]
source("parse.group.r")
ord <- bytype[[2]]
while(length(bl)){
  done<-c()
  for(i in seq_along(bl)){
    a<-as.data.frame(parse.group("<- (?<a>[a-z]+) . (?<b>[a-z]+)",bl[i]))
    if(exists(a$a)&exists(a$b)){
      eval(parse(text=bl[i]))
      done <- c(done, i)
    }
  }
  ord <- c(ord,bl[done])
  bl <- bl[-done]
}
root
#152479825094094

options(digits=22)
rm(list=ls())
input<-readLines("input21.txt")
input<-gsub(": "," <- ", input)
input[grepl("^root",input)]<-gsub("\\+","==",input[grepl("^root",input)])
input <- input[!grepl("^humn",input)]
bytype<-split(input, grepl("[0-9]", input))
eval(parse(text=bytype[[2]]))
bl <- bytype[[1]]
source("parse.group.r")
ord <- bytype[[2]]
while(length(bl)){ #First step as for part 1
  done<-c()
  for(i in seq_along(bl)){
    a<-as.data.frame(parse.group("<- (?<a>[a-z]+) .{1,2} (?<b>[a-z]+)",bl[i]))
    if(exists(a$a)&exists(a$b)){
      eval(parse(text=bl[i]))
      done <- c(done, i)
    }
  }
  if(!length(done)) break
  ord <- c(ord,bl[done])
  bl <- bl[-done]
}
rt <- bl[33] #Root statement
bl<-bl[-33]
while(length(bl)){ #Second step: replace variable names by their equations
  w <- el(regmatches(rt,gregexpr("[a-z]+",rt)))[-1] #FInd variable names in root statement
  for(x in w){
    if(exists(x)){
      rt <- gsub(x,as.character(get(x)),rt) #If already solved by part 1 just replace by its value
    }else{
      g <- grepl(paste0(x," <- "),bl) #Find eq
      rt <- gsub(x,paste0("(",gsub("^.+ <- ","",bl[g]),")"),rt) #replace in root statement
      bl <- bl[!g]
    }
  }
}
eq <- gsub("root <- ","",rt)
library(Ryacas)
library(glue)
yac_str(glue("Simplify({eq})"))
#"(396*(3360561285172-humn))/25==0"
#humn = 3360561285172
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

##Not gonna work
options(digits=22)
rm(list=ls())
input<-readLines("input21.txt")
input<-gsub(": "," <- ", input)
input[grepl("^root",input)]<-gsub("\\+","==",input[grepl("^root",input)])
input <- input[!grepl("^humn",input)]
#humn <- 0+1i
bytype<-split(input, grepl("[0-9]", input))
eval(parse(text=bytype[[2]]))
bl <- bytype[[1]]
source("parse.group.r")
ord <- bytype[[2]]
while(length(bl)){
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
rt <- bl[33]
bl<-bl[-33]
while(length(bl)){
  w <- el(regmatches(rt,gregexpr("[a-z]+",rt)))[-1]
  for(x in w){
    if(exists(x)){
      rt <- gsub(x,as.character(get(x)),rt)
    }else{
      g <- grepl(paste0(x," <- "),bl)
      rt <- gsub(x,paste0("(",gsub("^.+ <- ","",bl[g]),")"),rt)
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
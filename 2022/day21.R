options(digits=22)
input<-readLines("input21.txt")
input<-gsub(": "," <- ", input)
bytype<-split(input, grepl("[0-9]", input))
eval(parse(text=bytype[[2]]))
bl <- bytype[[1]]
source("parse.group.r")
while(length(bl)){
  done<-c()
  for(i in seq_along(bl)){
    a<-as.data.frame(parse.group("<- (?<a>[a-z]+) . (?<b>[a-z]+)",bl[i]))
    if(exists(a$a)&exists(a$b)){
      eval(parse(text=bl[i]))
      done <- c(done, i)
    }
  }
  bl <- bl[-done]
}
root
#152479825094094

##Not gonna work
input[grepl("^root",input)]<-gsub("\\+","==",input[grepl("^root",input)])
input <- input[!grepl("^humn",input)]
humn <- 0
root <- FALSE
while(!root){
  humn <- humn +1
  bytype<-split(input, grepl("[0-9]", input))
  eval(parse(text=bytype[[2]]))
  bl <- bytype[[1]]
  while(length(bl)){
    done<-c()
    for(i in seq_along(bl)){
      a<-as.data.frame(parse.group("<- (?<a>[a-z]+) .{1,2} (?<b>[a-z]+)",bl[i]))
      if(exists(a$a)&exists(a$b)){
        eval(parse(text=bl[i]))
        done <- c(done, i)
      }
    }
    bl <- bl[-done]
  }
  cat(humn,"\r")
}
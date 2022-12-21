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
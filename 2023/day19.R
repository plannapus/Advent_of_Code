input <- readLines(read.input(19))
# input <- readLines(textConnection("px{a<2006:qkq,m>2090:A,rfg}
# pv{a>1716:R,A}
# lnx{m>1548:A,A}
# rfg{s<537:gd,x>2440:R,A}
# qs{s>3448:A,lnx}
# qkq{x<1416:A,crn}
# crn{x>2662:A,R}
# in{s<1351:px,qqz}
# qqz{s>2770:qs,m<1801:hdj,R}
# gd{a>3333:R,R}
# hdj{m>838:A,pv}
# 
# {x=787,m=2655,a=1222,s=2876}
# {x=1679,m=44,a=2067,s=496}
# {x=2036,m=264,a=79,s=2244}
# {x=2461,m=1339,a=466,s=291}
# {x=2127,m=1623,a=2188,s=1013}"))
p <- gsub("\\}",")",gsub("\\{","c(",input[grepl("^\\{",input)]))
parts <- list()
for(i in seq_along(p)){
  parts[[i]] <- eval(parse(text=p[i]))
}
inst <- input[grepl("^[a-z]",input)]
inst <- gsub("([a-z]+)([,}])","\\1(y)\\2",inst)
inst <- gsub("\\{","<-function(y){attach(y);if(",inst)
inst <- gsub(":",")",inst)
inst <- gsub(",([a-z][><])"," else if(\\1",inst)
inst <- gsub(","," else ",inst)
inst <- gsub("^in<","first<",inst)
inst <- gsub("attach\\(y\\)","attach(y,warn.conflicts=FALSE)",inst)
R <- FALSE
A <- TRUE
eval(parse(text=inst))
kept <- c()
for(i in seq_along(parts)){
  kept[i] <- first(as.list(parts[[i]]))
  cat(i,"\r")
}
sum(unlist(parts[kept]))
#353046

inst <- input[grepl("^[a-z]",input)]
step1 <- do.call(rbind,strsplit(inst,"[}{]"))
step2 <- strsplit(step1[,2],",")
names(step2) <- step1[,1]

x <- m <- a <- s <- c(1,4000)
xmas <- list(x,m,a,s)
names(xmas) <- c("x","m","a","s")
f <- "in"
options(digits=22)
recurse <- function(f,xmas,i=1){
  if(f=="A"){
    tot <- (diff(xmas$x)+1)*(diff(xmas$m)+1)*(diff(xmas$a)+1)*(diff(xmas$s)+1)
    return(tot)
  }
  if(f=="R"){
    return(0)
  }
  cond <- step2[f][[1]][i]
  if(grepl(":",cond)){
    ele <- substr(cond,1,1)
    rest <- strsplit(substr(cond,3,nchar(cond)),":")[[1]]
    threshold <- as.integer(rest[1])
    dest <- rest[2]
    if(grepl(">",cond)){
      if(xmas[[ele]][1]<=threshold&xmas[[ele]][2]>threshold){
        xmas1 <- xmas2 <- xmas
        xmas1[[ele]][2] <- threshold
        xmas2[[ele]][1] <- threshold+1
        tot <- recurse(f,xmas1,i+1)+recurse(dest,xmas2,1)
      }else if(xmas[[ele]][1]>threshold){
        tot <- recurse(dest,xmas,1)
      }else{
        tot <- recurse(f,xmas,i+1)
      }
    }else{
      if(xmas[[ele]][1]<threshold&xmas[[ele]][2]>=threshold){
        xmas1 <- xmas2 <- xmas
        xmas1[[ele]][2] <- threshold-1
        xmas2[[ele]][1] <- threshold
        tot <- recurse(f,xmas2,i+1)+recurse(dest,xmas1,1)
      }else if(xmas[[ele]][1]>threshold){
        tot <- recurse(f,xmas,i+1)
      }else{
        tot <- recurse(dest,xmas,1)
      }
    }
  }else{
    tot <- recurse(cond,xmas,1)
  }
  return(tot)
}

recurse("in",xmas,1)
#125355665599537
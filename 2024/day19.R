input <- readLines(read.input(19))
towels <- strsplit(input[1],", ")[[1]]
designs <- input[3:402]

# towels <- c("r", "wr", "b", "g", "bwu", "rb", "gb", "br")
# 
# designs <- c("brwrr","bggr","gbbr","rrbgbr","ubwu","bwurrg","brgr","bbrgwb")

cache <- list()

check <- \(d){
  if(d%in%names(cache)) return(cache[d][[1]])
  queue <- c()
  for(i in seq_along(towels)){
    if(nchar(towels[i])<=nchar(d)){
      if(substr(d,1,nchar(towels[i]))==towels[i]) queue <- c(queue, towels[i]) 
    }
  }
  #cat(length(queue),"-",nchar(d),"\r")
  matched <- FALSE
  while(!matched){
    if(!length(queue)) break
    tw <- queue[1]
    if(tw==d){
      matched <- TRUE
    }else{
      matched <- check(substr(d,nchar(tw)+1,nchar(d)))
    }
    queue <- queue[-1]
  }
  cache[d] <<- matched
  return(matched)
}

#alld <- sapply(designs,check)
alld <- c()
for(i in seq_along(designs)){
  alld[i] <- check(designs[i])
  cat(i,"\r")
}
sum(alld)
#290


cache <- list()

count <- \(d){
  if(d%in%names(cache)) return(cache[d][[1]])
  queue <- c()
  for(i in seq_along(towels)){
    if(nchar(towels[i])<=nchar(d)){
      if(substr(d,1,nchar(towels[i]))==towels[i]) queue <- c(queue, towels[i]) 
    }
  }
  #cat(length(queue),"-",nchar(d),"\r")
  matched <- 0
  while(length(queue)){
    tw <- queue[1]
    if(tw==d){
      matched <- matched+1
    }else{
      matched <- matched + count(substr(d,nchar(tw)+1,nchar(d)))
    }
    queue <- queue[-1]
  }
  cache[d] <<- matched
  return(matched)
}

options(digits=22)
dd <- designs[alld]
#n <- sapply(dd,count)
n <- c()
for(i in seq_along(dd)){
  n[i] <- count(dd[i])
  cat(i,"\r")
}
sum(n)
#712058625427487
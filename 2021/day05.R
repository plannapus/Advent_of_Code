input <- readLines("input05.txt")
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parsed <- regexpr("^(?<x1>[0-9]+),(?<y1>[0-9]+) -> (?<x2>[0-9]+),(?<y2>[0-9]+)$", input, perl=TRUE)
coord <- as.data.frame(parse.one(input,parsed))
coord <- as.data.frame(apply(coord,2,as.integer))

h <- coord[coord$x1==coord$x2,]
v <- coord[coord$y1==coord$y2,]
map <- array(0,dim=c(1000,1000))
for(i in 1:nrow(h)){
  map[h$x1[i],h$y1[i]:h$y2[i]] <- map[h$x1[i],h$y1[i]:h$y2[i]]+1
}
for(i in 1:nrow(v)){
  map[v$x1[i]:v$x2[i],v$y1[i]] <- map[v$x1[i]:v$x2[i],v$y1[i]]+1
}
sum(map>1)
#7468
map <- array(0,dim=c(1000,1000))
for(i in 1:nrow(coord)){
  if(coord$x1[i]==coord$x2[i]){
    map[coord$x1[i],coord$y1[i]:coord$y2[i]] <- map[coord$x1[i],coord$y1[i]:coord$y2[i]]+1
  }else if(coord$y1[i]==coord$y2[i]){
    map[coord$x1[i]:coord$x2[i],coord$y1[i]] <- map[coord$x1[i]:coord$x2[i],coord$y1[i]]+1
  }else{
    line <- cbind(coord$x1[i]:coord$x2[i],coord$y1[i]:coord$y2[i])
    for(j in 1:nrow(line)){
      map[line[j,1],line[j,2]] <- map[line[j,1],line[j,2]] + 1
    }
  }
}
sum(map>1)
#22364
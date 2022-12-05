input <- readLines("input05.txt")
w <- grep("move",input)[1]
mat <- read.fwf(file="input05.txt",widths=rep(4,9),n=(w-3))
mat <- apply(mat,2,\(x)gsub("[][ ]","",x))
stacks <- list()
for(i in 1:ncol(mat)){
  stacks[[i]]<-mat[,i]
  stacks[[i]] <- stacks[[i]][stacks[[i]]!=""]
}
stacks2 <- stacks
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parsed <- regexpr("^move (?<n>[0-9]+) from (?<from>[0-9]+) to (?<to>[0-9]+)$", input[w:length(input)], perl=TRUE)
inst <- parse.one(input[w:length(input)],parsed)
inst <- apply(inst,2,as.integer)
inst <- as.data.frame(inst)
for(i in 1:nrow(inst)){
  substack <- stacks[[inst$from[i]]][1:inst$n[i]]
  stacks[[inst$from[i]]] <- stacks[[inst$from[i]]][-(1:inst$n[i])]
  stacks[[inst$to[i]]] <- c(rev(substack),stacks[[inst$to[i]]])
}
cat(sapply(stacks,el),sep="")
#FRDSQRRCD

for(i in 1:nrow(inst)){
  substack <- stacks2[[inst$from[i]]][1:inst$n[i]]
  stacks2[[inst$from[i]]] <- stacks2[[inst$from[i]]][-(1:inst$n[i])]
  stacks2[[inst$to[i]]] <- c(substack,stacks2[[inst$to[i]]])
}
cat(sapply(stacks2,el),sep="")
#HRFTQVWNN
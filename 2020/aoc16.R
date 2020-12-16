parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}

input <- readLines("input16.txt")
w <- which(input=="your ticket:")
w1 <- which(input=="nearby tickets:")
rules <- input[1:(w-2)]
parse.one()
parsed <- regexpr("^(?<name>[a-z ]+): (?<a1>[0-9]+)-(?<a2>[0-9]+) or (?<a3>[0-9]+)-(?<a4>[0-9]+)$", rules, perl=TRUE)
R <- parse.one(rules,parsed)
list_rules <- apply(R,1,function(x)c(x[2]:x[3],x[4]:x[5]))
names(list_rules) <- R[,1]
mine <- as.integer(el(strsplit(input[w+1],",")))
nearby <- do.call(rbind,lapply(strsplit(input[(w1+1):length(input)],","),as.integer))

## Part 1
sum(nearby[!nearby%in%unlist(list_rules)])
#28882

valids <- nearby[apply(nearby,1,function(x)all(x%in%unlist(list_rules))),]
table <- apply(valids,2,function(x)sapply(list_rules,function(y)all(x%in%y)))
fields <- rep(NA,length(list_rules))
names(fields) <- names(list_rules)
m <- ncol(table)
while(any(is.na(fields))){
  x <- which(rowSums(table)==1)
  y <- which(table[x,]==1)
  fields[names(x)]<-y
  table[,y]<-FALSE
  table[x,] <- FALSE
}
options(digits=22)
prod(mine[fields[grepl("^departure",names(fields))]])
#1429779530273

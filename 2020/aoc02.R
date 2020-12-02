### Part 1
input <- readLines("input02.txt")
#Function from the help file of regex, 
#also https://stackoverflow.com/a/29669403/1451109
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parsed <- regexpr("^(?<lb>[0-9]+)-(?<ub>[0-9]+) (?<let>[a-z]+): (?<p>[a-z]+)$", input, perl=TRUE)
tab <- parse.one(input,parsed)
tab <- as.data.frame(tab)
tab$lb <- as.integer(tab$lb)
tab$ub <- as.integer(tab$ub)

let_by_let  <- lapply(strsplit(tab$p,""),function(x)table(factor(x,levels=letters)))
u <- unlist(lapply(seq_along(let_by_let),function(i)let_by_let[[i]][letters==tab$let[i]]))
check <- names(u)
all(check==tab$let)
# TRUE
sum(u>=tab$lb&u<=tab$ub)
# 538

###Part 2
res <- c()
for(i in 1:nrow(tab)){
  l <- substr(tab$p[i],tab$lb[i],tab$lb[i])==tab$let[i]
  u <- substr(tab$p[i],tab$ub[i],tab$ub[i])==tab$let[i]
  res[i] <- l+u
}
sum(res==1)
#489
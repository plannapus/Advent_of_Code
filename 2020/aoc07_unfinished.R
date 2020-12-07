parse_reac = function(line){
  parse.one = function(res, result){
    m = do.call(rbind, lapply(seq_along(res), function(i) {
      if(result[i] == -1) return("")
      st <- attr(result, "capture.start")[i, ]
      substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
    }))
    colnames(m) = attr(result, "capture.names")
    m
  }
  if(grepl("no other bags.",line)){
    col <- gsub("^(.+) bags contain no other bags","\\1",line)
    res <- list(a=col,b=NULL)
  }else{
    spl <- strsplit(line," bags contain")[[1]]
    col <- spl[1]
    all <- strsplit(spl[2],", ")
    s = regexpr("(?<n>[0-9]+) (?<col>[a-z ]+) bag",all,perl=TRUE)
    reac = as.data.frame(parse.one(all,s))
    res <- list(a=col,b=reac)
  }
  res
}
input <- readLines("input07.txt")
res <- lapply(input,parse_reac)
col <- "shiny gold"
repeat{
  col_0 <- col
  col <- unique(c(col,unlist(sapply(res, function(x)if(!is.null(x$b)) x$a[any(col%in%x$b$col)]))))
  if(length(col_0)==length(col)) break
}
length(col)-1


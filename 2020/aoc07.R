## Part 1
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
    col <- gsub("^(.+) bags contain no other bags.","\\1",line)
    res <- list(a=col,b=NULL)
  }else{
    spl <- strsplit(line," bags contain")[[1]]
    col <- spl[1]
    all <- strsplit(spl[2],", ")
    s = do.call(rbind,lapply(all,function(x)parse.one(x,regexpr("(?<n>[0-9]+) (?<col>[a-z ]+) bag",x,perl=TRUE))))
    reac = as.data.frame(s)
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
#161

## Part 2
step <- res[sapply(res,function(x)x$a=="shiny gold")][[1]]$b
step$n<-as.integer(step$n)
step$end <- FALSE
repeat{
  replacement <- data.frame(n=NULL,col=NULL, end=NULL)
  for(i in 1:nrow(step)){
    if(!step$end[i]){
      sub <- res[sapply(res,function(x)x$a==step$col[i])][[1]]$b
      if(!is.null(sub)){
        sub$n <- as.integer(sub$n)*as.integer(step$n[i])
        sub$end <- FALSE
        step$end[i] <- TRUE
        replacement <- rbind(replacement, sub, step[i,])
      }else{
        step$end[i]<-TRUE
        replacement <- rbind(replacement, step[i,])
      }
    }else{
      replacement <- rbind(replacement, step[i,])
    }
  }
  step <- replacement
  if(all(step$end)){break}
}
sum(as.integer(step$n))
#30899

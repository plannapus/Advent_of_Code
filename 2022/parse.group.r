parse.one <- function(res, result) { #old faithful (from regexpr help file)
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}

parse.group <- function(regex, input){
  parsed <- regexpr(regex, input, perl=TRUE)
  parse.one(input, parsed)
}
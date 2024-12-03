parse.group <- function(regex,input) { #old faithful (from regexpr help file)
  result <- regexpr(regex, input, perl=TRUE)
  m <- do.call(rbind, lapply(seq_along(input), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(input[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  as.data.frame(m)
}

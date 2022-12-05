# Read in file
input <- readLines("input05.txt")
# Figure out where the instructions begin
w <- grep("move",input)[1]
# Read the crate stacks as a fixed width table
mat <- read.fwf(file="input05.txt",widths=rep(4,9),n=(w-3))
# Get rid of non integer characters and clean up
mat <- apply(mat,2,\(x)gsub("[][ ]","",x))
stacks <- list()
for(i in 1:ncol(mat)){
  stacks[[i]] <- mat[,i]
  stacks[[i]] <- stacks[[i]][stacks[[i]]!=""]
}
# Duplicate the initial setup for part 2
stacks2 <- stacks

# Parse the instructions
# Classic function to extract Perl-style named group in regex.
parse.one <- function(res, result) { #old faithful (from regexpr help file)
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
inst <- as.data.frame(apply(inst,2,as.integer))

# Part 1
for(i in 1:nrow(inst)){
  substack <- stacks[[inst$from[i]]][1:inst$n[i]]
  stacks[[inst$from[i]]] <- stacks[[inst$from[i]]][-(1:inst$n[i])]
  stacks[[inst$to[i]]] <- c(rev(substack),stacks[[inst$to[i]]])
}
cat(sapply(stacks,el),sep="")
#FRDSQRRCD

# Part 2
for(i in 1:nrow(inst)){
  substack <- stacks2[[inst$from[i]]][1:inst$n[i]]
  stacks2[[inst$from[i]]] <- stacks2[[inst$from[i]]][-(1:inst$n[i])]
  stacks2[[inst$to[i]]] <- c(substack,stacks2[[inst$to[i]]])
}
cat(sapply(stacks2,el),sep="")
#HRFTQVWNN
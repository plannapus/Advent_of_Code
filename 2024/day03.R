r <- paste0(readLines(read.input(3)),collapse="")
s <- strsplit(gsub("mul\\(|\\)","",regmatches(r,gregexpr("mul\\([0-9]+,[0-9]+\\)",r))[[1]]),",")
m <- apply(do.call(rbind,s),2,as.integer)
sum(apply(m,1,prod))
#173419328

inst <- regmatches(r,gregexpr("(mul\\([0-9]+,[0-9]+\\))|(do\\(\\))|(don\\'t\\(\\))",r))[[1]]

do <- TRUE
a <- 0
for(i in seq_along(inst)){
  if(inst[i]=="do()") do <- TRUE
  if(inst[i]=="don't()") do <- FALSE
  if(grepl("mul",inst[i])&do) a <- a + prod(as.integer(strsplit(gsub("(mul\\()|\\)","",inst[i]),",")[[1]]))
}
a
#90669332
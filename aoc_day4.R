#Day 4 Puzzle 1
input4 <- as.character(246540:787419)
w=grepl("([0-9])\\1",input4)
z=sapply(strsplit(input4,""),function(x)all(diff(as.integer(x))>=0))
sum(w&z)
#1063  

#Day 4 Puzzle 2
y=grepl("(^|[1-9])00($|[1-9])",input4)|
  grepl("(^|[02-9])11($|[02-9])",input4)|
  grepl("(^|[013-9])22($|[013-9])",input4)|
  grepl("(^|[0-24-9])33($|[0-24-9])",input4)|
  grepl("(^|[0-35-9])44($|[0-35-9])",input4)|
  grepl("(^|[0-46-9])55($|[0-46-9])",input4)|
  grepl("(^|[0-57-9])66($|[0-57-9])",input4)|
  grepl("(^|[0-68-9])77($|[0-68-9])",input4)|
  grepl("(^|[0-79])88($|[0-79])",input4)|
  grepl("(^|[0-8])99($|[0-8])",input4)
sum(y&z)
#686

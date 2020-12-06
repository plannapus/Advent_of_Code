## Part 1
input <- readLines("input06.txt")
input[input==""]<-"\n"
input <- paste(input, collapse=" ")
sum(sapply(sapply(strsplit(gsub(" ","",readLines(textConnection(input))),""),table),length))
#6633

## Part 2
nb_per_group <- sapply(strsplit(gsub("^ |  $","",readLines(textConnection(input)))," "), length)
answers <- sapply(strsplit(gsub(" ","",readLines(textConnection(input))),""),table)
sum(sapply(seq_along(answers), function(i)sum(answers[[i]]==nb_per_group[i])))
#3202
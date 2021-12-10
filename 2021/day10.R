input <- readLines("input10.txt")
#input <- readLines("test10.txt")
scores <- c(")"=3,"]"= 57,"}"=1197,">"=25137)

f <- function(i){
  while(grepl("(\\(\\))|(\\{\\})|(\\[\\])|(<>)",i)){
    i=gsub("(\\(\\))|(\\{\\})|(\\[\\])|(<>)","",i)
  }
  scores[head(el(strsplit(gsub("^[[({<]+","",i),"")),1)]
}
s <- sapply(input,f)
sum(unlist(s))
#323613

g <- function(i){
  while(grepl("(\\(\\))|(\\{\\})|(\\[\\])|(<>)",i)){
    i=gsub("(\\(\\))|(\\{\\})|(\\[\\])|(<>)","",i)
  }
  missingchar <- rev(chartr("([{<",")]}>",el(strsplit(i,""))))
  score <- 0
  for(j in seq_along(missingchar)){
    score <- score*5 + switch(missingchar[j],")"=1,"]"=2,"}"=3,">"=4)
  }
  score
}
input2 <- input[sapply(s,length)==0]
r <- sapply(input2,g)
median(r)
#3103006161
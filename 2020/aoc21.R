#Part1
input <- readLines("input21.txt")
allergenes <- gsub("^.+ \\(contains (.+)\\)$","\\1", input)
allergenes <- strsplit(allergenes,", ")
ingredients <- strsplit(gsub(" \\(contains .+\\)$","", input)," ")
allAll <- unique(unlist(allergenes))
ingAll <- list()
for(i in seq_along(allAll)){
  w <- sapply(allergenes,function(x)allAll[i]%in%x)
  ingAll[[i]] <- names(which(table(unlist(ingredients[w]))==sum(w)))
}

while(any(sapply(ingAll,length)>1)){
  S <- sapply(ingAll,length)==1
  x <- unlist(ingAll[S])
  for(i in seq_along(S)[!S]){
    ingAll[[i]] <- ingAll[[i]][!ingAll[[i]]%in%x]
  }
}
allergies <- cbind(allAll,unlist(ingAll))

ing <- unlist(ingredients)
length(ing[!ing%in%allergies[,2]])
#1913

#Part2
paste(allergies[order(allergies[,1]),2],collapse=",")
#gpgrb,tjlz,gtjmd,spbxz,pfdkkzp,xcfpc,txzv,znqbr

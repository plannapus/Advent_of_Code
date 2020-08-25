#Day 25 Puzzle 1
source("intcode_fast_but_dirty.R")
code = scan("input25.txt",sep=",")
A = intfast(code,c(),1,1,0,FALSE,FALSE)
# while(A$status){    #Manual exploration
#   cat(rawToChar(as.raw(A$out)))
#   r=scan(what=character(),n=1,sep="\n")
#   input=c(as.integer(charToRaw(r)),10)
#   A=intfast(A$op,input,A$n,1,A$rb,FALSE,FALSE)
# }
inst = readLines("instructions25_1.txt")
for(i in seq_along(inst)){
  cat(rawToChar(as.raw(A$out)))
  #r=readLines(file("stdin"),n=1)
  input=c(as.integer(charToRaw(inst[i])),10)
  A=intfast(A$op,input,A$n,1,A$rb,FALSE,FALSE)
}
library(gtools)
cat(rawToChar(as.raw(A$out)))
obj = c("loom","mug","spool of cat6","jam","manifold","food ration","fuel cell","prime number")
L=lapply(8:1,function(x)combinations(8,x,obj))
for(i in seq_along(L)){
  if(i==1){
    all_tests = "north\n"
  }else{
    for(j in 1:nrow(L[[i]])){
      miss = obj[!obj%in%L[[i]][j,]]
      all_tests = c(all_tests,sprintf("drop %s\n", miss),"north\n",sprintf("take %s\n",miss))
    }
  }
}
for(i in seq_along(all_tests)){
  cat(rawToChar(as.raw(A$out)))
  #r=readLines(file("stdin"),n=1)
  input=as.integer(charToRaw(all_tests[i]))
  A=intfast(A$op,input,A$n,1,A$rb,FALSE,FALSE)
  if(!grepl("ejected",rawToChar(as.raw(A$out)))){B = A}
}
cat(rawToChar(as.raw(B$out)))
#== Pressure-Sensitive Floor ==
# Analyzing...
# 
# Doors here lead:
#   - south
# 
# A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
# Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
# "Oh, hello! You should be able to get in by typing 537002052 on the keypad at the main airlock."

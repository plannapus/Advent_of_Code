#Day 21 Puzzle 1
code=scan("input21.txt",sep=",")
source("intcode_fast_but_dirty.R")
springscript = as.integer(charToRaw("NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n"))
A=intfast(code,springscript,1,1,0,FALSE,FALSE)
cat(tail(A$out,1))
#19352720

#Day 21 Puzzle 2
springscript= as.integer(charToRaw("NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT E T\nNOT T T\nOR H T\nAND T J\nRUN\n"))
A=intfast(code,springscript,1,1,0,FALSE,FALSE)
cat(tail(A$out,1))
#1143652885

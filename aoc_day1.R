#Day 1 Puzzle1
sum(sapply(scan("input.txt"),function(x)x%/%3-2))
#3488702

#Day 1 Puzzle2
sum(sapply(scan("input.txt"),function(x){
  y=0
  while(x>0){
    x = x%/%3-2
    if(x>0) y = y + x
  }
  y}))
#5230169

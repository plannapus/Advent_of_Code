#Day 19 Puzzle 1
code=scan("input19.txt",sep=",")
source("intcode_fast_but_dirty.R")
pts = expand.grid(0:49,0:49)
res = c()
for(i in 1:nrow(pts)){
  res[i] = as.integer(intfast(code,c(pts[i,1],pts[i,2]),1,1,0,FALSE,FALSE)$out)
  if(!i%%100)cat(i,"\r")
}
cat(sum(res==1))
#126

#Day 19 Puzzle 2
x=10
y=100
while(TRUE){
  if(as.integer(intfast(code,c(x,y),1,1,0,FALSE,FALSE)$out)){
    if(as.integer(intfast(code,c(x+99,y-99),1,1,0,FALSE,FALSE)$out)){
      break
    }else{
      y = y+1
    }
  }else{
    x = x+1
  }
  cat(x,":",y,"\r")
}
cat(x*10000+(y-99))
#11351625

#Day 16 Puzzle 1
input = readLines("input16.txt")
input = as.integer(el(strsplit(input,"")))
pattern=c(0,1,0,-1)
out = c()
steps=100
for(j in seq_len(steps)){
  for(i in 1:length(input)){
    out[i]=abs(sum(input*rep(rep(pattern,each=i),length=length(input)+1)[-1]))%%10
  }
  input = out
}
cat(out[1:8],sep="")
#84970726

#Day 16 Puzzle 2
input = readLines("input16.txt")
input = as.integer(el(strsplit(input,"")))
input=rep(input,10000)
offset = as.integer(paste(input[1:7],collapse=""))
pattern=c(1,0,-1,0)
steps=100
L=length(input)
a = input[(offset+1):L]
offset>L/2
#TRUE
for(j in seq_len(steps)){
  a = rev(abs(cumsum(rev(a)))%%10)
  cat(j,"\r")
}
cat(paste(a[1:8],collapse=""))
#47664469

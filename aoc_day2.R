#Day 2 Puzzle 1
f21=function(x){
  n=1
  while(x[n]!=99){
    if(x[n]==1){
      x[x[n+3]+1]=x[x[n+1]+1]+x[x[n+2]+1]
    } else if(x[n]==2){
      x[x[n+3]+1]=x[x[n+1]+1]*x[x[n+2]+1]
    }
    n <- n+4
  }
  x
}
a=scan("input2.txt",sep=",")
a[2]=12
a[3]=2
b=f21(a)
b[1]
# 6730673

#Day 2 Puzzle 2
a=scan("input2.txt",sep=",")
length(a)
eg <- expand.grid(0:148,0:148)
all <- apply(eg,1,function(x){b=a;b[2]=x[1];b[3]=x[2];d=f21(b);d[1]})
res <- eg[all==19690720,]
100*res[1] + res[2]
# 3749

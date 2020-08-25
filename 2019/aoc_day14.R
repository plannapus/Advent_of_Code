#Day 14 Puzzle 1
options(digits=22)
reactions = readLines("input14.txt")
parse_reac = function(line){
  parse.one = function(res, result){
    m = do.call(rbind, lapply(seq_along(res), function(i) {
      if(result[i] == -1) return("")
      st <- attr(result, "capture.start")[i, ]
      substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
    }))
    colnames(m) = attr(result, "capture.names")
    m
  }
  sol = strsplit(strsplit(line,"=> ")[[1]][2]," ")[[1]]
  r = strsplit(strsplit(line," =>")[[1]][1],",")[[1]]
  s = regexpr("(?<n>[0-9]+) (?<el>[A-Z]+)",r,perl=TRUE)
  reac = as.data.frame(parse.one(r,s),stringsAsFactors=FALSE)
  reac[,1] = as.integer(reac[,1])
  list(n=as.integer(sol[1]),a=sol[2],b=reac)
}
res = lapply(reactions,parse_reac)
orePerFuel = function(res,N){
  b = res[sapply(res,function(x)x$a=="FUEL")][[1]]$b
  b$n = b$n*N
  unused=data.frame(n=0,el="")
  while(any(!b$el%in%"ORE")){
    for(i in 1:nrow(b)){
      if(b$n[i]>0){
        if(b$el[i]!="ORE"){
          new_b = res[sapply(res,function(x)x$a==b$el[i])][[1]]$b
          new_n = res[sapply(res,function(x)x$a==b$el[i])][[1]]$n
          new_b$n=ceiling(b$n[i]/new_n)*new_b$n
          if(new_n*ceiling(b$n[i]/new_n)-b$n[i]>0){
            unused = rbind(unused,data.frame(n=new_n*ceiling(b$n[i]/new_n)-b$n[i],el=b$el[i]))
          }
          if(i==1){
            d=new_b
          }else{
            d = rbind(d,new_b)
            d = do.call(rbind,lapply(split(d,d$el),function(x)data.frame(n=sum(x$n),el=x$el[1],stringsAsFactors=FALSE)))
          }
        }else{
          if(b$el[i]%in%d$el){
            d$n[d$el==b$el[i]] = d$n[d$el==b$el[i]] + b$n[i]
          }else{
            d = rbind(d,b[i,])
          }
        }
      }
    }
    b=d
    for(i in 1:nrow(unused)){
      if(unused$el[i]%in%b$el){
        b$n[b$el==unused$el[i]] = b$n[b$el==unused$el[i]] - unused$n[i]
        unused$n[i]=unused$n[i]-b$n[b$el==unused$el[i]]
      }
    }
    unused = unused[unused$n>=0,]
    b = b[b$n>0,]
  }
  b$n
}
orePerFuel(res,1)
#1920219

#Day 14 Puzzle 2
Min = 1000000000000%/%orePerFuel(res,1)
Max = 10*Min
while(Max>Min+1){
  Mean = (Max+Min)%/%2
  if(orePerFuel(res,Mean) > 1000000000000){
    Max = Mean
  }else{
    Min = Mean
  }
}
Min
#1330066

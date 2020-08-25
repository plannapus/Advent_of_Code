#Day 7 Puzzle 1
f71=function(x,inputs){
  n=1
  m=1
  out = c()
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    if(op==1){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n = n+4
    } else if(op==2){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n = n+4
    } else if(op==3){
      if(!par[1]){x[x[n+1]+1]=inputs[m];m=m+1}else{x[n+1]=inputs[m];m=m+1}
      n = n+2
    } else if(op==4){
      out = c(out,ifelse(par[1],x[n+1],x[x[n+1]+1]))
      n = n+2
    } else if(op==5){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      if(p1){
        n = ifelse(par[2],x[n+2]+1,x[x[n+2]+1]+1)
      }else{
        n = n+3
      }
    } else if(op==6){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      if(!p1){
        n = ifelse(par[2],x[n+2]+1,x[x[n+2]+1]+1)
      }else{
        n = n+3
      }
    } else if(op==7){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      p2 = ifelse(par[2],x[n+2],x[x[n+2]+1])
      if(par[3]){
        x[n+3] = ifelse(p1<p2,1,0)
      }else{
        x[x[n+3]+1] = ifelse(p1<p2,1,0)
      }
      n = n+4
    } else if(op==8){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      p2 = ifelse(par[2],x[n+2],x[x[n+2]+1])
      if(par[3]){
        x[n+3] = ifelse(p1==p2,1,0)
      }else{
        x[x[n+3]+1] = ifelse(p1==p2,1,0)
      }
      n = n+4
    }
  }
  as.integer(paste(out,collapse=""))
}

phases = expand.grid(0:4,0:4,0:4,0:4,0:4)
ph = phases[apply(phases,1,function(x)!any(duplicated(x))),]
x = scan("input7.txt",what=integer(),sep=",")
res = c()
for(j in 1:nrow(ph)){
  start=0
  phase = unlist(ph[j,])
  for(i in seq_along(phase)){
    start = f71(x,c(phase[i],start))
  }
  res[j] = start
}
max(res)
#21860

#Day 7 Puzzle 2
f72=function(x,inputs,n=1,m=1){
  out = c()
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    if(op==1){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n = n+4
    } else if(op==2){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n = n+4
    } else if(op==3){
      if(m>length(inputs)){
        return(list(op=x,out=as.integer(paste(out,collapse="")),n=n,m=m,status=1))
      }else{
        if(!par[1]){x[x[n+1]+1]=inputs[m];m=m+1}else{x[n+1]=inputs[m];m=m+1}
        n = n+2
      }
    } else if(op==4){
      out = c(out,ifelse(par[1],x[n+1],x[x[n+1]+1]))
      n = n+2
    } else if(op==5){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      if(p1){
        n = ifelse(par[2],x[n+2]+1,x[x[n+2]+1]+1)
      }else{
        n = n+3
      }
    } else if(op==6){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      if(!p1){
        n = ifelse(par[2],x[n+2]+1,x[x[n+2]+1]+1)
      }else{
        n = n+3
      }
    } else if(op==7){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      p2 = ifelse(par[2],x[n+2],x[x[n+2]+1])
      if(par[3]){
        x[n+3] = ifelse(p1<p2,1,0)
      }else{
        x[x[n+3]+1] = ifelse(p1<p2,1,0)
      }
      n = n+4
    } else if(op==8){
      p1 = ifelse(par[1],x[n+1],x[x[n+1]+1])
      p2 = ifelse(par[2],x[n+2],x[x[n+2]+1])
      if(par[3]){
        x[n+3] = ifelse(p1==p2,1,0)
      }else{
        x[x[n+3]+1] = ifelse(p1==p2,1,0)
      }
      n = n+4
    }
  }
  list(op=x,out=as.integer(paste(out,collapse="")),n=n,m=m,status=0)
}

phases = expand.grid(5:9,5:9,5:9,5:9,5:9)
ph = phases[apply(phases,1,function(x)!any(duplicated(x))),]
x = scan("input7.txt",what=integer(),sep=",")
res = c()
for(j in 1:nrow(ph)){
  phase = unlist(ph[j,])
  A = f72(x,c(phase[1],0))
  B = f72(x,c(phase[2],A$out))
  C = f72(x,c(phase[3],B$out))
  D = f72(x,c(phase[4],C$out))
  E = f72(x,c(phase[5],D$out))
  while(A$status & B$status & C$status & D$status & E$status){
    A = f72(A$op,E$out,A$n)
    B = f72(B$op,A$out,B$n)
    C = f72(C$op,B$out,C$n)
    D = f72(D$op,C$out,D$n)
    E = f72(E$op,D$out,E$n)
  }
  res[j] = E$out
}
max(res)
# 2645740

#Day 5 Puzzle 1
f51=function(x){
  n=1
  out <- c()
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    #cat(sprintf("op:%i\npar:%i %i %i\nparam:%s\n",op,par[1],par[2],par[3],ifelse(op%in%1:2,paste(x[n+1],x[n+2],x[n+3]),x[n+1])))
    if(op==1){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])+ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n <- n+4
    } else if(op==2){
      if(par[3]){
        x[n+3]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }else{
        x[x[n+3]+1]=ifelse(par[1],x[n+1],x[x[n+1]+1])*ifelse(par[2],x[n+2],x[x[n+2]+1])
      }
      n <- n+4
    } else if(op==3){
      if(!par[1]){x[x[n+1]+1]=1}else{x[n+1]=1}
      n <- n+2
    } else if(op==4){
      out <- c(out,ifelse(par[1],x[n+1],x[x[n+1]+1]))
      n <- n+2
    }
  }
  as.integer(paste(out,collapse=""))
}
a=scan("input5.txt",sep=",")
f51(a)
#15259545

#Day 5 Puzzle 2
f52=function(x){
  n=1
  out <- c()
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    #cat(sprintf("op:%i\npar:%i %i %i\nparam:%s\n",op,par[1],par[2],par[3],ifelse(op%in%c(1:2,7:8),paste(x[n+1],x[n+2],x[n+3]),ifelse(op%in%3:4,x[n+1],paste(x[n+1],x[n+2])))))
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
      if(!par[1]){x[x[n+1]+1]=as.integer(readline("Input:"))}else{x[n+1]=as.integer(readline("Input:"))}
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
      p1 <- ifelse(par[1],x[n+1],x[x[n+1]+1])
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

f52(a)
#Input:5
#7616021

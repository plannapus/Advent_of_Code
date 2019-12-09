intfast=function(X,inputs=c(),n=1,m=1,rbase=0,verbose=FALSE){
  options(digits=22)
  x <- rep(0,1e6)
  x[seq_along(X)]=X
  out <- 0
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    if(verbose) cat(sprintf("op:%i\npar:%i %i %i\nrbase:%i\nparam:%s\n",
                            op,par[1],par[2],par[3],rbase,
                            `if`(op%in%c(1:2,7:8),paste(x[n+1],x[n+2],x[n+3]),
                                 `if`(op%in%c(3:4,9),x[n+1],paste(x[n+1],x[n+2])))))
    if(op==1){
      pos = `if`(par[3]==1,n+3,x[n+3]+1+`if`(par[3]==2,rbase,0))
      el1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      el2 = `if`(par[2]==1,x[n+2],x[x[n+2]+1+`if`(par[2]==2,rbase,0)])
      x[pos] = el1 + el2
      if(verbose) cat(sprintf("Changed position %i to %s.\n",pos,el1 + el2))
      n = n+4
    } else if(op==2){
      pos = `if`(par[3]==1,n+3,x[n+3]+1+`if`(par[3]==2,rbase,0))
      el1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      el2 = `if`(par[2]==1,x[n+2],x[x[n+2]+1+`if`(par[2]==2,rbase,0)])
      x[pos] = el1 * el2
      if(verbose) cat(sprintf("Changed position %i to %s.\n",pos, el1 * el2))
      n = n+4
    } else if(op==3){
      if(m>length(inputs)){
        return(list(op=x,out=paste(out,collapse=""),n=n,m=m,status=1))
      }else{
        pos = `if`(par[1]==1,n+1,x[n+1]+1+`if`(par[1]==2,rbase,0))
        x[pos] = inputs[m]
        m = m +1
        if(verbose) cat(sprintf("Changed position %i to %s.\n",pos,x[pos]))
        n = n+2
      }
    } else if(op==4){
      el1 = as.character(`if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)]))
      out = c(out,el1)
      if(verbose) cat(sprintf("Output: %s\n",el1))
      n = n+2
    } else if(op==5){
      p1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      if(p1){
        n = `if`(par[2]==1,x[n+2]+1,x[x[n+2]+1+`if`(par[2]==2,rbase,0)]+1)
      }else{
        n = n+3
      }
      if(verbose) cat(sprintf("Changed n to %i.\n",n))
    } else if(op==6){
      p1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      if(!p1){
        n = `if`(par[2]==1,x[n+2]+1,x[x[n+2]+1+`if`(par[2]==2,rbase,0)]+1)
      }else{
        n = n+3
      }
      if(verbose) cat(sprintf("Changed n to %i.\n",n))
    } else if(op==7){
      p1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      p2 = `if`(par[2]==1,x[n+2],x[x[n+2]+1+`if`(par[2]==2,rbase,0)])
      pos = `if`(par[3]==1,n+3,x[n+3]+1+`if`(par[3]==2,rbase,0))
      x[pos] = `if`(p1<p2,1,0)
      if(verbose) cat(sprintf("Changed position %i to %s.\n",pos,x[pos]))
      n = n+4
    } else if(op==8){
      p1 = `if`(par[1]==1,x[n+1],x[x[n+1]+1+`if`(par[1]==2,rbase,0)])
      p2 = `if`(par[2]==1,x[n+2],x[x[n+2]+1+`if`(par[2]==2,rbase,0)])
      pos = `if`(par[3]==1,n+3,x[n+3]+1+`if`(par[3]==2,rbase,0))
      x[pos] = `if`(p1==p2,1,0)
      if(verbose) cat(sprintf("Changed position %i to %s.\n",pos, x[pos]))
      n = n+4
    }else if(op==9){
      pos = `if`(par[1]==1,n+1,x[n+1]+1+`if`(par[1]==2,rbase,0))
      rbase = rbase + x[pos]
      n = n+2
      if(verbose) cat(sprintf("Changed relative base to %i.\n",rbase))
    }
  }
  list(op=x,out=out[-1],n=n,m=m,status=0)
}

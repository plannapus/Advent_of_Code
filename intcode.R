intcode=function(X,inputs=c(),n=1,m=1,rbase=0,verbose=FALSE){
  options(digits=22)
  x = rep(0,1e6)
  x[seq_along(X)] = X
  out = 0
  position = function(a,n,x){`if`(par[a]==1,n+a,x[n+a]+1+`if`(par[a]==2,rbase,0))}
  value = function(a,n,x){`if`(par[a]==1,x[n+a],x[x[n+a]+1+`if`(par[a]==2,rbase,0)])}
  while(x[n]%%100!=99){
    op = x[n]%%100
    par = (x[n]%/%10^(2:4))%%10
    n_par = seq_len(`if`(op%in%5:6,2,`if`(op%in%c(3:4,9),1,3)))
    if(verbose) cat(sprintf("----------\nop:%i\npar:%s\nrbase:%i\nparam:%s\n",
                            op,paste(par[n_par],collapse=" "),rbase,
                            paste(x[n+n_par],collapse=" ")))
    if(!op%in%c(4:6)) pos = position(tail(n_par,1),n,x)
    if(!op%in%c(3,9)){
      el1 = value(1,n,x)
      if(op!=4) el2 = value(2,n,x)
    }
    if(op==1){
      x[pos] = el1 + el2
      n = n+4
    } else if(op==2){
      x[pos] = el1 * el2
      n = n+4
    } else if(op==3){
      if(m>length(inputs)){
        return(list(op=x,out=paste(out[-1],collapse=""),n=n,m=m,status=1))
      }else{
        x[pos] = inputs[m]
        m = m +1
        n = n+2
      }
    } else if(op==4){
      el1 = as.character(el1)
      out = c(out,el1)
      n = n+2
    } else if(op==5){
      n = `if`(el1,el2+1,n+3)
    } else if(op==6){
      n = `if`(el1,n+3,el2+1)
    } else if(op==7){
      x[pos] = as.integer(el1<el2)
      n = n+4
    } else if(op==8){
      x[pos] = as.integer(el1==el2)
      n = n+4
    }else if(op==9){
      rbase = rbase + x[pos]
      n = n+2
    }
    if(verbose){
      if(op%in%c(1:3,7:8)) cat(sprintf("Changed position %i to %s.\n",pos,x[pos]))
      if(op==4) cat(sprintf("%s added to output.\n",el1))
      if(op%in%5:6) cat(sprintf("Changed n to %i.\n",n))
      if(op==9) cat(sprintf("Changed relative base to %i.\n",rbase))
    }
  }
  return(list(op=x,out=paste(out[-1],collapse=""),n=n,m=m,status=0))
}

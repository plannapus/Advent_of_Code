#Day 24 Puzzle 1
options(digits=22)
map = do.call(rbind,strsplit(readLines("input24.txt"),""))
new_state = function(map){
  eg = expand.grid(seq_len(nrow(map)),seq_len(ncol(map)))
  new_map = map
  neigh = matrix(c(0,1,0,-1,1,0,-1,0),ncol=2,byrow=TRUE)
  for(i in 1:nrow(eg)){
    content_n = table(factor(unlist(apply(neigh,1,function(x)if((x[1]+eg[i,1]>0 & x[2]+eg[i,2]>0)&(x[1]+eg[i,1]<=nrow(map) & x[2]+eg[i,2]<=ncol(map)))map[x[1]+eg[i,1],x[2]+eg[i,2]])),c(".","#")))
    content = map[eg[i,1],eg[i,2]]
    new_map[eg[i,1],eg[i,2]] = ifelse(content=="#" & content_n['#']!=1,'.', ifelse(content=="." & content_n['#']%in%1:2,"#",content))
  }
  new_map
}

id = function(map){paste(as.integer(map=="#"),collapse="")}

biodiv_rate = function(map){
  a = (seq_len(nrow(map)*ncol(map))-1)[t(map)=="#"]
  sum(2^a)
}
A=list(map)
n=1
repeat{
  map = new_state(map)
  n = n+1
  A[[n]]=map
  ids = sapply(A,id)
  if(any(duplicated(ids))) break
}
biodiv_rate(map)
#32776479

#Day 24 Puzzle 2
map = do.call(rbind,strsplit(readLines("input24.txt"),""))
map[3,3]="?"
M = list(list(level=0, map=map))
for(i in 1:200){
  levels = sort(sapply(M,function(x)x$level))
  P = list()
  p = 1
  for(j in seq_along(levels)){
    lmap = M[sapply(M,function(x)x$level)==levels[j]][[1]]$map
    eg = expand.grid(seq_len(nrow(lmap)),seq_len(ncol(lmap)))
    eg = eg[!(eg[,1]==3 & eg[,2]==3),]
    new_map = lmap
    neigh = matrix(c(0,1,0,-1,1,0,-1,0),ncol=2,byrow=TRUE)
    for(k in 1:nrow(eg)){
      nb = apply(neigh,1,function(x)c(x[1]+eg[k,1],x[2]+eg[k,2]))
      content_n = sum(apply(nb,2,function(x){
        n = 0
        if((x[1]>0 & x[1]<6) & (x[2]>0 & x[2]<6)){
          if(x[1]==3 & x[2]==3 & (levels[j]+1) %in% levels){
            if(eg[k,1]==2) n = sum(M[sapply(M,function(x)x$level)==levels[j]+1][[1]]$map[1,]=="#")
            if(eg[k,1]==4) n = sum(M[sapply(M,function(x)x$level)==levels[j]+1][[1]]$map[5,]=="#")
            if(eg[k,2]==2) n = sum(M[sapply(M,function(x)x$level)==levels[j]+1][[1]]$map[,1]=="#")
            if(eg[k,2]==4) n = sum(M[sapply(M,function(x)x$level)==levels[j]+1][[1]]$map[,5]=="#")
          }else{
            n = lmap[x[1],x[2]]=="#"
          }
        }
        if((levels[j]-1) %in% levels){
          if(x[1]==0) n = M[sapply(M,function(x)x$level)==levels[j]-1][[1]]$map[2,3]=="#"
          if(x[2]==0) n = M[sapply(M,function(x)x$level)==levels[j]-1][[1]]$map[3,2]=="#"
          if(x[1]==6) n = M[sapply(M,function(x)x$level)==levels[j]-1][[1]]$map[3,4]=="#"
          if(x[2]==6) n = M[sapply(M,function(x)x$level)==levels[j]-1][[1]]$map[4,3]=="#"
        }
        as.integer(n)
      }))
      content = lmap[eg[k,1],eg[k,2]]
      new_map[eg[k,1],eg[k,2]] = ifelse(content=="#" & content_n!=1,'.', ifelse(content=="." & content_n%in%1:2,"#",content))
    }
    P[[p]] = list(level = levels[j], map = new_map)
    p = p+1
    if(!(levels[j]+1) %in% levels){
      lower_map = matrix(".",ncol=5,nrow=5)
      lower_map[3,3]="?"
      if(lmap[3,2]=="#") lower_map[,1] = "#"
      if(lmap[2,3]=="#") lower_map[1,] = "#"
      if(lmap[3,4]=="#") lower_map[,5] = "#"
      if(lmap[4,3]=="#") lower_map[5,] = "#"
      if(sum(lower_map=="#")){
        P[[p]] = list(level=levels[j]+1, map = lower_map)
        p = p+1
      }
    }
    if(!(levels[j]-1) %in% levels){
      upper_map = matrix(".",ncol=5,nrow=5)
      upper_map[3,3]="?"
      if(sum(lmap[,1]=="#")%in%1:2) upper_map[3,2] = "#"
      if(sum(lmap[1,]=="#")%in%1:2) upper_map[2,3] = "#"
      if(sum(lmap[,5]=="#")%in%1:2) upper_map[3,4] = "#"
      if(sum(lmap[5,]=="#")%in%1:2) upper_map[4,3] = "#"
      if(sum(upper_map=="#")){
        P[[p]] = list(level=levels[j]-1, map =upper_map)
        p=p+1
      }
    }
  }
  M = P
  cat(i,":",sum(sapply(M,function(x)sum(x$map=="#"))),"\r")
}
sum(sapply(M,function(x)sum(x$map=="#")))

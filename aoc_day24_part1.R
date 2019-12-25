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

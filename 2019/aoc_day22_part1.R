#Day 22 Puzzle 1
instructions = readLines("input22.txt")
Cut = function(stack,n){
  c(tail(stack,-1*n),head(stack,n))
}

Increment = function(stack,n){
  newstack = rep(NA,length(stack))
  i=1
  while(length(i)!=length(stack)){i = c(i,1+(tail(i,1)+n-1)%%length(stack))}
  newstack[i]=stack
  newstack
}

stack = 0:10006
for(i in seq_along(instructions)){
  if(grepl("^cut",instructions[i])){
    n = as.integer(regmatches(instructions[i],gregexpr("[-0-9]+",instructions[i]))[[1]])
    stack = Cut(stack, n)
  }else if(grepl("increment",instructions[i])){
    n = as.integer(regmatches(instructions[i],gregexpr("[0-9]+",instructions[i]))[[1]])
    stack = Increment(stack,n)
  }else if(grepl("new",instructions[i])){
    stack = rev(stack)
  }
}
which(stack==2019)-1
#2604

#Day 22 Puzzle 2
options(digits=22)
ldeck = 119315717514047
position = 2020
rinst = rev(instructions)
trials = 101741582076661
sp=position

InvIncrement = function(position,ldeck,n){
  i=1
  while(tail(i,1)!=position){i = c(i,1+(tail(i,1)+n-1)%%ldeck)}
  length(i)
}
t = 1
while(t<trials){
  for(i in seq_along(instructions)){
    if(grepl("new",rinst[i])){
      position = ldeck-position+1
    }else if(grepl("cut",rinst[i])){
      n = as.integer(regmatches(rinst[i],gregexpr("[0-9]+",rinst[i]))[[1]])
      if(n<0){
        if(abs(n)<position){
          position = position - n
        }else{
          position = position + n
        }
      }else{
        if(n>position){
          position = position + n
        }else{
          position = position - n
        }
      }
    }else if(grepl("increment",rinst[i])){
      n = as.integer(regmatches(rinst[i],gregexpr("[0-9]+",rinst[i]))[[1]])
      position = InvIncrement(position,ldeck,n)
    }
  }
  sp=c(sp,position)
  cat(t,"\r")
  if(position==2020){break}
  t = t+1
}
sp[trials%%t]
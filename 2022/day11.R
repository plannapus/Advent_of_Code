#Hardcoding the monkeys
monkeys <- list(
  list(items=c(54,53),
       op=function(x){x*3},
       test=function(x){ifelse(x%%2,6,2)},
       n=0),
  list(items=c(95, 88, 75, 81, 91, 67, 65, 84),
       op=function(x){x*11},
       test=function(x){ifelse(x%%7,4,3)},
       n=0),
  list(items=c(76, 81, 50, 93, 96, 81, 83),
       op=function(x){x+6},
       test=function(x){ifelse(x%%3,1,5)},
       n=0),
  list(items=c(83, 85, 85, 63),
       op=function(x){x+4},
       test=function(x){ifelse(x%%11,4,7)},
       n=0),
  list(items=c(85, 52, 64),
       op=function(x){x+8},
       test=function(x){ifelse(x%%17,7,0)},
       n=0),
  list(items=c(57),
       op=function(x){x+2},
       test=function(x){ifelse(x%%5,3,1)},
       n=0),
  list(items=c(60, 95, 76, 66, 91),
       op=function(x){x*x},
       test=function(x){ifelse(x%%13,5,2)},
       n=0),
  list(items=c(65, 84, 76, 72, 79, 65),
       op=function(x){x+5},
       test=function(x){ifelse(x%%19,0,6)},
       n=0)
)

monkey_copy <- monkeys

for(t in 1:20){
  for(m in 1:8){
    for(i in seq_along(monkeys[[m]]$items)){
      worry <- floor(monkeys[[m]]$op(monkeys[[m]]$items[i])/3)
      next_m <- monkeys[[m]]$test(worry)+1
      monkeys[[next_m]]$items <- c(monkeys[[next_m]]$items,worry)
      monkeys[[m]]$n <- monkeys[[m]]$n+1
    }
    monkeys[[m]]$items <- c()
  }
}

prod(tail(sort(sapply(monkeys,function(x)x$n)),2))
#111210

library(bit64)
monkeys <- monkey_copy
for(t in 1:10000){
  for(m in 1:8){
    if(length(monkeys[[m]]$items)){
    for(i in seq_along(monkeys[[m]]$items)){
      worry <- monkeys[[m]]$op(monkeys[[m]]$items[i])%%(2*7*3*11*17*5*13*19)
      next_m <- monkeys[[m]]$test(worry)+1
      monkeys[[next_m]]$items <- c(monkeys[[next_m]]$items,worry)
      monkeys[[m]]$n <- monkeys[[m]]$n+1
    }
    monkeys[[m]]$items <- c()
  }}
  if(!t%%100)cat(t,"\r")
}
prod(tail(sort(sapply(monkeys,function(x)x$n)),2))
#15447387620
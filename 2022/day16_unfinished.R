input <- readLines("input16.txt")
source("parse.group.R")
input <- as.data.frame(parse.group("Valve (?<v>[A-Z]+) has flow rate=(?<f>[0-9]+); tunnels? leads? to valves? (?<l>[ ,A-Z]+)",input))
input$f <- as.integer(input$f)
P <- strsplit(input$l,", ")
edg <- matrix(ncol=2,nrow=0)
for(i in seq_along(P)){
  edg <- rbind(edg,cbind(input$v[i],P[[i]]))
}
library(igraph)
g<-graph_from_edgelist(edg)
D <- distances(g,input$v,input$v)

search <- function(t,from="AA",v=input$v[input$f!=0]){
  res <- c()
  for(i in seq_along(v)){
    if(D[from,v[i]]<t){
      res[i] <- input$f[input$v==v[i]] * (t-D[from,v[i]]-1)
      if(length(v[-i])) res[i] <- res[i]+search(t-D[from,v[i]]-1,v[i],v[-i])
    }else{
      res[i]<-0
    }
  }
  return(max(res,na.rm=TRUE))
}

search(30)
#2119

# search2 <- function(t1,t2,from1="AA",from2="AA",v=input$v[input$f!=0]){
#   if(length(v)==1){
#     if(D[from1,v[i]]<t1 & D[from1,v[i]]<D[from2,v[i]]){
#       res <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1)
#     }else if(D[from2,v[i]]<t2){
#       res <- input$f[input$v==v[i]] * (t2-D[from2,v[i]]-1)
#     }else if(D[from1,v[i]]<t1){
#       res <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1)
#     }else{
#       res <- 0
#     }
#   }else{
#     res <- matrix(0,nrow=length(v),ncol=length(v))
#     for(i in seq_along(v)){
#       for(j in seq_along(v)[-i]){
#         if(D[from1,v[i]]<t1 & D[from2,v[j]]<t2){
#           res[i,j] <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1) +
#             input$f[input$v==v[j]] * (t2-D[from2,v[j]]-1)
#           if(length(v[-c(i,j)])){
#             res[i,j] <- res[i,j]+search2(t1-D[from1,v[i]]-1,
#                                          t2-D[from1,v[j]]-1,
#                                          v[i],v[j],v[-c(i,j)])
#           }
#         }else if(D[from1,v[i]]<t1 & D[from2,v[j]]>t2){
#           res[i,j] <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1)
#           if(length(v[-i])){
#             res[i,j] <- res[i,j]+search2(t1-D[from1,v[i]]-1,
#                                          t2,
#                                          v[i],from2,v[-i])
#           }
#         }else if(D[from1,v[i]]>=t1 & D[from2,v[j]]<t2){
#           res[i,j] <- input$f[input$v==v[j]] * (t2-D[from2,v[j]]-1)
#           if(length(v[-j])){
#             res[i,j] <- res[i,j]+search2(t1,
#                                          t2-D[from2,v[j]]-1,
#                                          from1,v[j],v[-j])
#           }
#         }
#       }
#     }
#   }
#   return(max(res,na.rm=TRUE))
# }
# 
# search2(26,26)

flow <- function(path, time){
  current <- "AA"
  res <- 0
  for(i in path){
    time <- time - D[current,i] -1
    res <- res + input$f[input$v==i]*time
    current <- i
  }
  res
}

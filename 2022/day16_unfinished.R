input <- readLines("input16.txt")
source("parse.group.r")
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

search2 <- function(t1,t2,from1="AA",
                    from2="AA",v=input$v[input$f!=0]){
  res <- matrix(0,nrow=length(v),ncol=length(v))
  for(i in seq_along(v)){
    v3 <- v[!v%in%v[i]]
    for(j in seq_along(v3)){
      #cat(v1[i],"-",v3[j],"\n")
      #browser()
        if(D[from1,v[i]]<t1&D[from2,v3[j]]<t2){
          res[i,j] <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1) +
                      input$f[input$v==v3[j]] * (t2-D[from2,v3[j]]-1)
          if(length(v[-i])&length(v3[-j])){
            res[i,j] <- res[i,j]+search2(t1-D[from1,v1[i]]-1,t2-D[from2,v3[j]]-1,
                                    v[i],v3[j],v[!v%in%c(v[i],v3[j])])
          }else if(length(v[-i])){
            res[i,j] <- res[i,j]+search(t1-D[from1,v[i]]-1,v[i],v[-i])
          }else if(length(v3[-j])){
            res[i,j] <- res[i,j]+search(t2-D[from2,v3[j]]-1,v3[j],v3[-j])
          }
        }else if(D[from1,v[i]]<t1){
          res[i,j] <- input$f[input$v==v[i]] * (t1-D[from1,v[i]]-1)
          if(length(v[-i])&length(v3)){
            res[i,j] <- res[i,j]+search2(t1-D[from1,v1[i]]-1,t2,
                                         v1[i],from2,v[-i])
          }else if(length(v[-i])){
            res[i,j] <- res[i,j]+search(t1-D[from1,v[i]]-1,v[i],v[-i])
          }else if(length(v3)){
            res[i,j] <- res[i,j]+search(t2,from2,v3)
          }
        }else if(D[from2,v3[j]]<t2){
          res[i,j] <- input$f[input$v==v3[j]] * (t2-D[from2,v3[j]]-1)
          if(length(v)&length(v3[-j])){
            res[i,j] <- res[i,j]+search2(t1,t2-D[from2,v3[j]]-1,
                                         from1,v3[j],v[!v%in%v3[j]])
          }else if(length(v)){
            res[i,j] <- res[i,j]+search(t1,from1,v)
          }else if(length(v3[-j])){
            res[i,j] <- res[i,j]+search(t2-D[from2,v3[j]]-1,v3[j],v3[-j])
          }
      }else{
        res[i,j]<-0
      }
    }
  }
  return(max(res,na.rm=TRUE))
}

search2(26,26)
#2615
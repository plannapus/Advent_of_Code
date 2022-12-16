input <- readLines("test16.txt")
input <- as.data.frame(parse.group("Valve (?<v>[A-Z]+) has flow rate=(?<f>[0-9]+); tunnels? leads? to valves? (?<l>[ ,A-Z]+)",input))
input$f <- as.integer(input$f)
P <- strsplit(input$l,", ")
edg <- matrix(ncol=2,nrow=0)
for(i in seq_along(P)){
  edg <- rbind(edg,cbind(input$v[i],P[[i]]))
}
library(igraph)
g<-graph_from_edgelist(edg)
#sp <- all_simple_paths(g,from="AA") #<-Does not work because do not allow loops

flow<-function(path){ #<-find 1712 for example!
  i<-1
  totflow <- 0
  time <- 0
  opened <- c()
  for(i in seq_along(path)){
    if(input$f[input$v%in%path[i]]>0&!path[i]%in%opened){
      #First turn on valve
      opened <- c(opened, path[i])
      totflow <- totflow+(30-time)*input$f[input$v==path[i]]
      time <- time+2
    }else{
      #Just move
      time <- time+1
    }
  }
  return(list(totflow=totflow,time=time,opened=opened))
}

step <- function(from,path){
  path <- c(path,from)
  fl <- flow(path)
  time <- fl$time
  if(time>=30){
    return(list(fl$totflow,path))
  }
  if(length(fl$opened)>5){
    return(list(fl$totflow,path))
  }
  nextstep <- P[[which(input$v==from)]]
  st <- list()
  for(i in seq_along(nextstep)){
    st[[i]] <- step(nextstep[i],path)
  }
  return(st[[which.max(sapply(st,`[`,1))]])
}

step("AA",c())

# step <- function(opened, time, current){ #Does not work either!!!
#   if(time<=0) return(0)
#   best <- 0
#   if(!current %in% opened){
#     flow <- (time-1)*input$f[input$v==current]
#     opened <- c(opened, current)
#     for(nextstep in P[[which(input$v==current)]]){
#       if(flow!=0){
#         best <- pmax(best, flow + step(opened, time - 2, nextstep))
#       }else{
#         best <- pmax(best, step(opened, time - 1, nextstep))
#       }
#     }
#   }else{
#     for(nextstep in P[[which(input$v==current)]]){
#       best <- pmax(best, step(opened, time - 1, nextstep))
#     }
#   }
#   return(best)
# }
# 
# step(c(),30,"AA")

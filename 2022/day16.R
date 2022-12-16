input <- readLines("input16.txt")
input <- as.data.frame(parse.group("Valve (?<v>[A-Z]+) has flow rate=(?<f>[0-9]+); tunnels? leads? to valves? (?<l>[ ,A-Z]+)",input))
input$f <- as.integer(input$f)
P <- strsplit(input$l,", ")
edg <- matrix(ncol=2,nrow=0)
for(i in seq_along(P)){
  edg <- rbind(edg,cbind(input$v[i],P[[i]]))
}
library(igraph)
g<-graph_from_edgelist(edg)
sp <- all_simple_paths(g,from="AA") #<-Does not work because do not allow loops

flow<-function(path){
  totflow <- 0
  flowt <- 0
  time <- 0
  i<-1
  while(time<=30&i<=length(path)){
    if(input$f[input$v%in%names(path)[i]]>0){
      #First turn on valve
      totflow <- totflow+flowt
      flowt <- flowt+input$f[input$v==names(path)[i]]
      time <- time+1
      #Then move
      totflow <- totflow+flowt
      i <- i+1
      time <- time+1
    }else{
      #Just move
      totflow <- totflow+flowt
      i <- i+1
      time <- time+1
    }
  }
  totflow <- totflow +(30-time)*flowt
  return(totflow)
}
all_flow <- c()
for(i in seq_along(sp)){
  all_flow[i] <- flow(sp[[i]])
}
max(all_flow)

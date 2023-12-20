input <- strsplit(readLines(read.input(20))," -> ")[1:58]
# input <- strsplit(readLines(textConnection("broadcaster -> a
# %a -> inv, con
# &inv -> b
# %b -> con
# &con -> output"))," -> ")
inst <- lapply(input,strsplit,split=", ")
names <- sapply(inst,\(x)gsub("^[&%]","",x[1]))
type <- sapply(inst,\(x)ifelse(x[1]=="broadcaster",
                               "broadcaster",
                               gsub("[a-z]+","",x[1])))
dest <- lapply(inst,\(x)x[[2]])
flips <- rep(FALSE,length(type))
remember <- list()
for(i in 1:length(names)){
  o <- names[sapply(dest,\(x)names[i]%in%x)]
  if(length(o)) remember[[i]] <- data.frame(origin=o,signal=0)
}


signal <- c()
from <- c()
to <- c()
i <- 1
for(j in 1:1000){
  signal <- c(signal,0)
  from <- c(from, "button")
  to <- c(to, "broadcaster")
  while(i<=length(signal)){
    if(!to[i]%in%names){
      i <- i+1
    }else{
      remember[[which(names==to[i])]]$signal[remember[[which(names==to[i])]]$origin==from[i]] <- signal[i]
      if(type[names==to[i]]=="%"){
        if(!signal[i]){
          flips[names==to[i]] <- !flips[names==to[i]]
          if(flips[names==to[i]]){
            newsignal <- 1
          }else{
            newsignal <- 0
          }
        }else{
          i <- i+1
          next
        }
      }else if(type[names==to[i]]=="&"){
        if(all(remember[[which(names==to[i])]]$signal==1)){
          newsignal <- 0
        }else{
          newsignal <- 1
        }
      }else{
        newsignal <- signal[i]
      }
      newd <- dest[names==to[i]][[1]]
      from <- c(from, rep(to[i],length(newd)))
      to <- c(to, newd)
      signal <- c(signal,rep(newsignal,length(newd)))
      i <- i+1 
    }
    #if(!i%%1000)cat(i,"-",length(res),"\r")
  }
}
prod(table(signal))
#841763884

flips <- rep(FALSE,length(type))
remember <- list()
for(i in 1:length(names)){
  o <- names[sapply(dest,\(x)names[i]%in%x)]
  if(length(o)) remember[[i]] <- data.frame(origin=o,signal=0)
}

signal <- c()
from <- c()
to <- c()
i <- 1
n <- names[sapply(dest,\(x)"rx"%in%x)]
nn <- names[sapply(dest,\(x)n%in%x)]
res <- rep(NA,length(nn))
names(res) <- nn
for(j in 1:4200){
  signal <- c(signal,0)
  from <- c(from, "button")
  to <- c(to, "broadcaster")
  while(i<=length(signal)){
    if(!to[i]%in%names){
      i <- i+1
    }else{
      remember[[which(names==to[i])]]$signal[remember[[which(names==to[i])]]$origin==from[i]] <- signal[i]
      if(type[names==to[i]]=="%"){
        if(!signal[i]){
          flips[names==to[i]] <- !flips[names==to[i]]
          if(flips[names==to[i]]){
            newsignal <- 1
          }else{
            newsignal <- 0
          }
        }else{
          i <- i+1
          next
        }
      }else if(type[names==to[i]]=="&"){
        if(all(remember[[which(names==to[i])]]$signal==1)){
          newsignal <- 0
        }else{
          newsignal <- 1
          if(to[i]%in%nn){
            res[to[i]] <- j
            print(res)
          }
        }
      }else{
        newsignal <- signal[i]
      }
      newd <- dest[names==to[i]][[1]]
      from <- c(from, rep(to[i],length(newd)))
      to <- c(to, newd)
      signal <- c(signal,rep(newsignal,length(newd)))
      i <- i+1 
    }
    if(!j%%10)cat(j,"\r")
  }
}
options(digits=22)
prod(res)
#246006621493687
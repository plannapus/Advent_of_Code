input <- readLines("input19.txt")
#input <- readLines("test19.txt")
source("parse.group.r")
input <- parse.group("Blueprint (?<n>[0-9]+): Each ore robot costs (?<or_or>[0-9]+) ore. Each clay robot costs (?<or_cl>[0-9]+) ore. Each obsidian robot costs (?<or_ob>[0-9]+) ore and (?<cl_ob>[0-9]+) clay. Each geode robot costs (?<or_ge>[0-9]+) ore and (?<ob_ge>[0-9]+) obsidian.",input)
input <- apply(input,2,as.integer)
input <- as.data.frame(input)
res <- rep(0,nrow(input))

for(i in 1:nrow(input)){
  cat("\nBlueprint", i,"\n")
  queue <- data.frame(t=0,
                      or=0,cl=0,ob=0,ge=0,
                      orbot=1,clbot=0,obbot=0,gebot=0)
  mo <- max(c(input$or_cl[i],input$or_ob[i],input$or_ge[i],input$or_or[i]))
  while(!24%in%queue$t){
    newqueue <- lapply(1:nrow(queue),\(x){data.frame(t=NULL,
                                                     or=NULL,cl=NULL,ob=NULL,ge=NULL,
                                                     orbot=NULL,clbot=NULL,obbot=NULL,gebot=NULL)})
    for(j in 1:nrow(queue)){
      state <- queue[j,]
      state2 <- state
      state2$or <- state$or+state$orbot
      state2$cl <- state$cl+state$clbot
      state2$ob <- state$ob+state$obbot
      state2$ge <- state$ge+state$gebot
      state2$t <- state$t + 1
      if(state$or>=input$or_ge[i] & state$ob>=input$ob_ge[i]){
        state2$or <- state2$or - input$or_ge[i]
        state2$ob <- state2$ob - input$ob_ge[i]
        state2$gebot <- state$gebot + 1
        newqueue[[j]] <- rbind(newqueue[[j]],state2)
      }else if(state$or>=input$or_ob[i] & state$cl>=input$cl_ob[i] & 
               state$obbot < input$ob_ge[i]){
        state2$or <- state2$or - input$or_ob[i]
        state2$cl <- state2$cl - input$cl_ob[i]
        state2$obbot <- state2$obbot + 1
        newqueue[[j]] <- rbind(newqueue[[j]],state2)
      }else if(state$or>=input$or_cl[i]|state$or>=input$or_or[i]){
        if(state$or>=input$or_cl[i]& 
           state$clbot < input$cl_ob[i]){
          state3 <- state2
          state3$or <- state3$or - input$or_cl[i]
          state3$clbot <- state3$clbot + 1
          newqueue[[j]] <- rbind(newqueue[[j]],state3)
        }
        if(state$or>=input$or_or[i] & 
           state$orbot<mo){
          state3 <- state2
          state3$or <- state3$or - input$or_or[i]
          state3$orbot <- state3$orbot + 1
          newqueue[[j]] <- rbind(newqueue[[j]],state3)
        }
        newqueue[[j]] <- rbind(newqueue[[j]],state2) #Case where nothing is done
      }else{
        newqueue[[j]] <- rbind(newqueue[[j]],state2) #Case where nothing can be done
      }
      if(!j%%100|j==nrow(queue)) cat(unique(queue$t),":",j,"\r")
    }
    newqueue <- do.call(rbind,newqueue)
    newqueue <- newqueue[!duplicated(newqueue),]
    newqueue <- newqueue[!(newqueue$t==23 & newqueue$gebot==0),,drop=FALSE]
    newqueue <- newqueue[!(newqueue$t==22 & newqueue$obbot==0),,drop=FALSE]
    #cat("\n",nrow(newqueue),"\n")
    queue <- newqueue
    if(nrow(queue)==0) break
  }
  res[i] <- ifelse(nrow(queue)==0,0,max(queue$ge))
}
cat(sum(res*input$n))
#1681

res <- rep(0,3)

for(i in 1:3){
  cat("\nBlueprint", i,"\n")
  queue <- data.frame(t=0,
                      or=0,cl=0,ob=0,ge=0,
                      orbot=1,clbot=0,obbot=0,gebot=0)
  mo <- max(c(input$or_cl[i],input$or_ob[i],input$or_ge[i],input$or_or[i]))
  while(!32%in%queue$t){
    newqueue <- lapply(1:nrow(queue),\(x){data.frame(t=NULL,
                                                     or=NULL,cl=NULL,ob=NULL,ge=NULL,
                                                     orbot=NULL,clbot=NULL,obbot=NULL,gebot=NULL)})
    for(j in 1:nrow(queue)){
      state <- queue[j,]
      state2 <- state
      state2$or <- state$or+state$orbot
      state2$cl <- state$cl+state$clbot
      state2$ob <- state$ob+state$obbot
      state2$ge <- state$ge+state$gebot
      state2$t <- state$t + 1
      if(state$or>=input$or_ge[i] & state$ob>=input$ob_ge[i]){
        state2$or <- state2$or - input$or_ge[i]
        state2$ob <- state2$ob - input$ob_ge[i]
        state2$gebot <- state$gebot + 1
        newqueue[[j]] <- rbind(newqueue[[j]],state2)
      }else if(state$or>=input$or_ob[i] & state$cl>=input$cl_ob[i] & 
               state$obbot < input$ob_ge[i]){
        state2$or <- state2$or - input$or_ob[i]
        state2$cl <- state2$cl - input$cl_ob[i]
        state2$obbot <- state2$obbot + 1
        newqueue[[j]] <- rbind(newqueue[[j]],state2)
      }else if(state$or>=input$or_cl[i]|state$or>=input$or_or[i]){
        if(state$or>=input$or_cl[i]& 
           state$clbot < input$cl_ob[i]){
          state3 <- state2
          state3$or <- state3$or - input$or_cl[i]
          state3$clbot <- state3$clbot + 1
          newqueue[[j]] <- rbind(newqueue[[j]],state3)
        }
        if(state$or>=input$or_or[i] & 
           state$orbot<mo){
          state3 <- state2
          state3$or <- state3$or - input$or_or[i]
          state3$orbot <- state3$orbot + 1
          newqueue[[j]] <- rbind(newqueue[[j]],state3)
        }
        newqueue[[j]] <- rbind(newqueue[[j]],state2) #Case where nothing is done
      }else{
        newqueue[[j]] <- rbind(newqueue[[j]],state2) #Case where nothing can be done
      }
      if(!j%%100|j==nrow(queue)) cat(unique(queue$t),":",j,"\r")
    }
    newqueue <- do.call(rbind,newqueue)
    newqueue <- newqueue[!duplicated(newqueue),]
    newqueue <- newqueue[!(newqueue$t==31 & newqueue$gebot==0),,drop=FALSE]
    newqueue <- newqueue[!(newqueue$t==30 & newqueue$obbot==0),,drop=FALSE]
    #cat("\n",nrow(newqueue),"\n")
    queue <- newqueue
    if(nrow(queue)==0) break
  }
  res[i] <- ifelse(nrow(queue)==0,0,max(queue$ge))
}
cat(prod(res))
#5394
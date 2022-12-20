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
  while(nrow(queue)){
    if(any(queue$t==24)){
      done <- queue[queue$t==24,]
      queue <- queue[queue$t<24,,drop=FALSE]
      res[i] <- pmax(res[i],max(done$ge))
    }else if(any(queue$t==23 & queue$gebot==0)){
      queue <- queue[!(queue$t==23 & queue$gebot==0),,drop=FALSE]
    }else if(any(queue$t>19 &queue$obbot==0)){#Min obs is 7
      queue <- queue[!(queue$t>16 & queue$obbot==0),,drop=FALSE]
    }else if(any(queue$t>14 &queue$clbot==0)){#Min obs is 7
      queue <- queue[!(queue$t>11 & queue$clbot==0),,drop=FALSE]
    }else{
      state <- queue[1,]
      queue <- queue[-1,,drop=FALSE]
      state$or <- state$or+state$orbot
      state$cl <- state$cl+state$clbot
      state$ob <- state$ob+state$obbot
      state$ge <- state$ge+state$gebot
      state$t <- state$t + 1
      if(state$or>=input$or_ge[i] & state$ob>=input$ob_ge[i]){
        state$or <- state$or - input$or_ge[i]
        state$ob <- state$ob - input$ob_ge[i]
        state$gebot <- state$gebot + 1
        queue <- rbind(queue,state)
      }else if(state$or>=input$or_ob[i] & state$cl>=input$cl_ob[i] & 
               state$obbot <= input$ob_ge[i]){
        state$or <- state$or - input$or_ob[i]
        state$cl <- state$cl - input$cl_ob[i]
        state$obbot <- state$obbot + 1
        queue <- rbind(queue,state)
      }else if(state$or>=input$or_cl[i]|state$or>=input$or_or[i]){
        if(state$or>=input$or_cl[i]& 
                state$clbot <= input$cl_ob[i]){
        state3 <- state
        state3$or <- state3$or - input$or_cl[i]
        state3$clbot <- state3$clbot + 1
        queue <- rbind(queue,state3)
        }
        if(state$or>=input$or_or[i] & 
               state$orbot<=mo){
        state2 <- state
        state2$or <- state2$or - input$or_or[i]
        state2$orbot <- state2$orbot + 1
        queue <- rbind(queue,state2)
        }
        queue <- rbind(state,queue) #Case where nothing is done
      }else{
        queue <- rbind(state,queue) #Case where nothing can be done
      }
      queue <- queue[!duplicated(queue),,drop=FALSE]
      res[i] <- max(c(res[i],queue$ge))
    }
    cat(min(queue$t),"-",max(queue$t),":",res[i],"\r")
  }
}
cat(sum(res*input$n))



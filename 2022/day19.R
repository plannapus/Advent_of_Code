input <- readLines("input19.txt")
source("parse.group.r")
input <- parse.group("Blueprint (?<n>[0-9]+): Each ore robot costs (?<or_or>[0-9]+) ore. Each clay robot costs (?<or_cl>[0-9]+) ore. Each obsidian robot costs (?<or_ob>[0-9]+) ore and (?<cl_ob>[0-9]+) clay. Each geode robot costs (?<or_ge>[0-9]+) ore and (?<ob_ge>[0-9]+) obsidian.",input)
input <- apply(input,2,as.integer)
input <- as.data.frame(input)
res <- 0
state <- data.frame(or=rep(0,nrow(input)),
                    cl=rep(0,nrow(input)),
                    ob=rep(0,nrow(input)),
                    ge=rep(0,nrow(input)),
                    or_bot=rep(1,nrow(input)),
                    cl_bot=rep(0,nrow(input)),
                    ob_bot=rep(0,nrow(input)),
                    ge_bot=rep(0,nrow(input)))
for(i in 1:nrow(input)){
  for(t in 1:24){
    state$or[i] <- state$or[i]+state$or_bot[i]
    state$cl[i] <- state$cl[i]+state$cl_bot[i]
    state$ob[i] <- state$ob[i]+state$ob_bot[i]
    state$ge[i] <- state$ge[i]+state$ge_bot[i]
    if(state$or[i]>=input$or_ge[i] & state$ob[i]>=input$ob_ge[i]){
      state$or[i] <- state$or[i] - input$or_ge[i]
      state$ob[i] <- state$ob[i] - input$ob_ge[i]
      state$ge_bot[i] <- state$ge_bot[i] + 1
    }
    if(state$or[i]>=input$or_ob[i] & state$cl[i]>=input$cl_ob[i]){
      state$or[i] <- state$or[i] - input$or_ob[i]
      state$cl[i] <- state$cl[i] - input$cl_ob[i]
      state$ob_bot[i] <- state$ob_bot[i] + 1
    }
    if(state$or[i]>=input$or_or[i] & t<=input$or_cl[i]){
      state$or[i] <- state$or[i] - input$or_or[i]
      state$or_bot[i] <- state$or_bot[i] + 1
    }
    if(state$or[i]>=input$or_cl[i]){
      state$or[i] <- state$or[i] - input$or_cl[i]
      state$cl_bot[i] <- state$cl_bot[i] + 1
    }
  }
  res <- res + input$n[i]*state$ge[i]
}
#3896 #Answer is too high apparently
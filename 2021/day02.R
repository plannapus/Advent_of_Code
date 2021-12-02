input <- read.table("input02.txt",sep=" ")
by_instructions <- split(input[,2],input[,1])
summed<- sapply(by_instructions,sum)
summed[2]*(summed[1]-summed[3])
#1693300

aim <- 0
x <- 0
y <- 0
for(i in 1:nrow(input)){
  if(input[i,1]=="down") aim <- aim + input[i,2]
  if(input[i,1]=="up") aim <- aim - input[i,2]
  if(input[i,1]=="forward"){
    x <- x + input[i,2]
    y <- y + aim*input[i,2]
  }
}
x*y
#1857958050
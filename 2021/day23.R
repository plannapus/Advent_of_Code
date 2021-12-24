#Part 1 by hand: 13066
#Part 2 by hand: 47328

#(Unfinished) attempt to do it programmatically
input<-readLines("input23.txt")
scores <- c("A"=1,"B"=10,"C"=100,"D"=1000)
input <- c(input[1:3],"  #D#C#B#A#","  #D#B#A#C#",input[4:5])
#cat(input,sep="\n")

hallway <- rep("",11)
nostop <- c(3,5,7,9)
roomcontent <- list("A"=c("A","D","D","B"),
                    "B"=c("C","C","B","D"),
                    "C"=c("C","B","A","A"),
                    "D"=c("D","A","C","B"))

can_move_hw <- function(hallway,from,to){
  res <- TRUE
  if(to%in%nostop) res <- FALSE
  if(any(hallway[nostop[LETTERS[1:4]==from]:to]!="")) res <- FALSE
  return(res)
}

can_move_room <- function(currentcontent,hallway,from,to){
  res <- TRUE
  if(length(currentcontent[[to]])!=0) if(any(currentcontent[[to]]!=to)) res <- FALSE
  if(any(hallway[from:nostop[LETTERS[1:4]==to]]!="")) res <- FALSE
  return(res)
}

count_steps <- function(currentcontent,from,to){
  if(from%in%1:11 & to%in%LETTERS){
    steps <- abs(nostop[LETTERS[1:4]==to]-from)+4-length(currentcontent[to])
  }else{
    steps <- 5-length(currentcontent[[from]])+abs(nostop[LETTERS[1:4]==from]-to)
  }
  steps
}

done <- function(currentcontent){
  identical(currentcontent$A,rep("A",4))&
  identical(currentcontent$B,rep("B",4))&
  identical(currentcontent$C,rep("C",4))&
  identical(currentcontent$D,rep("D",4))
}

mover <- function(currentcontent,hallway){
  if(done(currentcontent)) return(list(cost=0,hallway=hallway,currentcontent=currentcontent))
  best_cost <- +Inf
  for(j in LETTERS[1:4]){
    if(length(currentcontent[[j]])!=0){
    if(!all(currentcontent[[j]]%in%j)){
    for(i in 1:11){
      if(!i%in%nostop & length(currentcontent[[j]])!=0){
      if(can_move_hw(hallway,j,i)){
        cost <- count_steps(currentcontent,j,i)
        hallway[i] <- currentcontent[[j]][1]
        if(length(currentcontent[[j]]>1)){
          currentcontent[[j]]<- currentcontent[[j]][-1]
        }else{
          currentcontent[[j]] <- c()
          }
        mv <- mover(currentcontent,hallway)
        next_cost <- mv$cost
        hallway <- mv$hall
        currentcontent <- mv$current
        new_cost <- cost + next_cost
        if(new_cost<best_cost) best_cost <- new_cost
      }}
    }
    }}}
  for(i in 1:11){
    if(hallway[i]!=""){
      if(can_move_room(currentcontent,hallway,i,hallway[i])){
        cost <- count_steps(currentcontent,i,hallway[i])
        currentcontent[[hallway[i]]] <- c(hallway[i],currentcontent[hallway[i]])
        hallway[i] <- ""
        mv <- mover(currentcontent,hallway)
        next_cost <- mv$cost
        hallway <- mv$hall
        currentcontent <- mv$current
        new_cost <- cost + next_cost
        if(new_cost<best_cost) best_cost <- new_cost
      }
    }
  }
  list(cost = best_cost, hallway=hallway, currentcontent = currentcontent)
}
mover(roomcontent,hallway)


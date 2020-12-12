##Part1
input <- readLines("input12.txt")
#input <- c("F10","N3","F7","R90","F11")
dir <- substr(input,1,1)
amount <- as.integer(substr(input,2,4))
toward <- "E"
w <- c("E","S","W","N","E","S","W","N","E")
pos <- c(0,0)
for(i in seq_along(dir)){
  if(dir[i]=="E"){
    pos[1] <- pos[1]+amount[i]
  }else if(dir[i]=="W"){
    pos[1] <- pos[1]-amount[i]
  }else if(dir[i]=="N"){
    pos[2] <- pos[2]+amount[i]
  }else if(dir[i]=="S"){
    pos[2] <- pos[2]-amount[i]
  }else if(dir[i]=="R"){
    toward <- w[which(w==toward)[1]+amount[i]%/%90]
  }else if(dir[i]=="L"){
    toward <- w[tail(which(w==toward),1)-amount[i]%/%90]
  }else if(dir[i]=="F"){
    pos <- pos + amount[i]*switch(toward,"E"=c(1,0),"W"=c(-1,0),"N"=c(0,1),"S"=c(0,-1))
  }
}
sum(abs(pos))
#1152

##Part2
pos <- c(0,0)
waypoint <- c(10,1)
for(i in seq_along(dir)){
  if(dir[i]=="E"){
    waypoint[1] <- waypoint[1]+amount[i]
  }else if(dir[i]=="W"){
    waypoint[1] <- waypoint[1]-amount[i]
  }else if(dir[i]=="N"){
    waypoint[2] <- waypoint[2]+amount[i]
  }else if(dir[i]=="S"){
    waypoint[2] <- waypoint[2]-amount[i]
  }else if(dir[i]=="R"){
    if(amount[i]==90) waypoint <- c(1,-1)*rev(waypoint)
    if(amount[i]==180) waypoint <- c(-1,-1)*waypoint
    if(amount[i]==270) waypoint <- c(-1,1)*rev(waypoint)
  }else if(dir[i]=="L"){
    if(amount[i]==270) waypoint <- c(1,-1)*rev(waypoint)
    if(amount[i]==180) waypoint <- c(-1,-1)*waypoint
    if(amount[i]==90) waypoint <- c(-1,1)*rev(waypoint)
  }else if(dir[i]=="F"){
    pos <- pos + amount[i]*waypoint
  }
}
sum(abs(pos))
#58637
#Day 12 Puzzle 1
r=readLines("input12.txt")
start=do.call(rbind,lapply(regmatches(r,gregexpr("[-0-9]+",r)),as.integer))
positions=velocities=array(dim=c(nrow(start),ncol(start),1001))
positions[,,1]=start
velocities[,,1]=0
for(t in 2:1001){
  for(i in 1:nrow(start)){
    positions[i,,t] = positions[i,,t-1]+colSums(t(apply(positions[,,t-1],1,function(x)sign(x-positions[i,,t-1]))))+velocities[i,,t-1]
    velocities[i,,t] = positions[i,,t]-positions[i,,t-1]
  }
}
potential = apply(abs(positions[,,t]),1,sum)
kinetic = apply(abs(velocities[,,t]),1,sum)
total = sum(potential*kinetic)
#7722

#Day 12 Puzzle 2
library(pracma)
r=readLines("input12.txt")
start=do.call(rbind,lapply(regmatches(r,gregexpr("[-0-9]+",r)),as.integer))
position=start
velocity=matrix(0,nrow=nrow(start),ncol=ncol(start))
#lcm = function(a,b){q=1:(a*b);min(q[!q%%a+q%%b])}
lcm = function(a,b) a*b/gcd(a,b)
cycle=rep(NA,3)
for(i in 1:3){
  newvel = rowSums(sapply(position[,i],function(x)sign(x-position[,i])))
  position[,i] = position[,i] + newvel + velocity[,i]
  velocity[,i] = newvel + velocity[,i]
  t = 1
  while(!all(c(position[,i],velocity[,i])==c(start[,i],c(0,0,0,0)))){
    t = t+1
    velocity[,i] = rowSums(sapply(position[,i],function(x)sign(x-position[,i]))) + velocity[,i]
    position[,i] = position[,i] + velocity[,i]
    #if(!t%%1000) cat(t,"\r")
  }
  cycle[i] = t
  #cat(i,"done\n")
}
options(digits=22)
lcm(lcm(cycle[1],cycle[2]),cycle[3])
#292653556339368

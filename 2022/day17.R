flow <- scan("input17.txt","")
shapes <- list(cbind(1,c(3,4,5,6)),
               cbind(c(1,2,2,2,3),c(4,3,4,5,4)),
               cbind(c(1,1,1,2,3),c(3,4,5,5,5)),
               cbind(c(1,2,3,4),3),
               cbind(c(1,1,2,2),c(3,4,3,4)))

k <- 1
tower <- matrix(ncol=2,nrow=0)
height <- 0
height_when_fallen <- rep(NA,2022)
k_when_fallen <- rep(NA,2022)
for(i in 1:2022){
  s <- shapes[[((i-1)%%5)+1]]
  s[,1] <- s[,1]+height+3
  while(TRUE){
    dir <- substr(flow,(k-1)%%nchar(flow)+1,(k-1)%%nchar(flow)+1)
    if(dir==">"){
      if(!7%in%s[,2]){
        test <- s
        test[,2]<-test[,2]+1
        if(!anyDuplicated(rbind(test,tower))){
          s <- test
        }
      }
    }
    if(dir=="<"){
      if(!1%in%s[,2]){
        test <- s
        test[,2]<-test[,2]-1
        if(!anyDuplicated(rbind(test,tower))){
          s <- test
        }
      }
    }
    k <- k+1
    test <- s
    test[,1] <- test[,1]-1
    if(!anyDuplicated(rbind(test,tower))&!any(test==0)){
      s <- test
    }else{
      tower <- rbind(tower,s)
      height <- max(tower[,1])
      height_when_fallen[i]<-height
      k_when_fallen[i] <- k
      break
    }
  }
  if(!i%%100)cat(i,"\r")
}
height
#3168
kwf <- (k_when_fallen%%nchar(flow))+1
cycle <- Reduce(intersect,lapply(split(kwf,1:2022%%5),
                                 function(x)which(duplicated(x))))[1]*5
first <- which(kwf == kwf[cycle])[1]
times <- (M-first) %/% (cycle-first)
rest <- height_when_fallen[as.integer((M-first)%%(cycle-first)+first)]
rest+times*(height_when_fallen[cycle]-height_when_fallen[first])

#1554117647070
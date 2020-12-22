##Part1
input <- readLines("input22.txt")
w <- grep("Player",input)
p1 <- scan(text=paste(input[(w[1]+1):(w[2]-2)],collapse="\n"))
p2 <- scan(text=paste(input[(w[2]+1):length(input)],collapse="\n"))
n<-0
while(length(p1)&length(p2)){
  n <- n+1
  if(p1[1]>p2[1]){
    p1 <- c(p1[-1],p1[1],p2[1])
    p2 <- p2[-1]
  }else{
    p2 <- c(p2[-1],p2[1],p1[1])
    p1 <- p1[-1]
  }
  cat(n,"\r")
}
sum(p2*rev(seq_along(p2)))
#33393

##Part2
p1 <- scan(text=paste(input[(w[1]+1):(w[2]-2)],collapse="\n"))
p2 <- scan(text=paste(input[(w[2]+1):length(input)],collapse="\n"))

game <- function(p1,p2){
  history = list()
  n = 1
  history[[n]]<-c(paste(p1,collapse=","),paste(p2,collapse=","))
  while(length(p1)&length(p2)){
    n = n+1
    x = p1[1]
    y = p2[1]
    p1 = p1[-1]
    p2 = p2[-1]
    if(length(p1)>=x&length(p2)>=y){
      subg = game(p1[1:x],p2[1:y])
      if(subg$winner==1){
        p1 = c(p1,x,y)
      }else{
        p2 = c(p2,y,x)
      }
    }else{
      if(x>y){
        p1 = c(p1,x,y)
      }else{
        p2 = c(p2,y,x)
      }
    }
    history[[n]] = c(paste(p1,collapse=","),paste(p2,collapse=","))
    check = sapply(history[-n],function(x)identical(x,history[[n]])|identical(rev(x),history[[n]]))
    if(any(check)){
      #cat("1")
      return(list(winner=1, p1=p1, p2=p2))
    }
    #if(length(p1)+length(p2)==50) cat("M(",length(p1),",",length(p2),")",sep="")
  }
  #cat(ifelse(length(p1),1,2))
  return(list(winner=ifelse(length(p1),1,2), p1=p1, p2=p2))
}
g <- game(p1,p2)
#cat("\n\n")
if(g$w==1) cat(sum(g$p1*rev(seq_along(g$p1))))
if(g$w==2) cat(sum(g$p2*rev(seq_along(g$p2))))
#31963
source("read.input.R")
options(digits=22)
input <- readLines(read.input(24))
cat(gsub(" @ ",", ",input),sep="\n",file="input24.txt")
input <- read.table("input24.txt",sep=",")
# input <- read.table(textConnection("19, 13, 30, -2,  1, -2
# 18, 19, 22, -1, -1, -2
# 20, 25, 34, -2, -2, -4
# 12, 31, 28, -1, -2, -1
# 20, 19, 15,  1, -5, -3"),sep=",")
window <- c(200000000000000,400000000000000)
# window <- c(7,27)
eg <- t(combn(1:nrow(input),2))
intersection<-\(x1,y1,vx1,vy1,
                x2,y2,vx2,vy2){
  x3<-x1+40*vx1 #because of arbitrary precision if >40 result is 12016, when 10 it s 12013 and when 1 it s 12008 go figure...
  y3<-y1+40*vy1
  x4<-x2+40*vx2
  y4<-y2+40*vy2
  px <- ((x1*y3-y1*x3)*(x2-x4)-(x1-x3)*(x2*y4-x4*y2))/((x1-x3)*(y2-y4)-(y1-y3)*(x2-x4))
  py <- ((x1*y3-y1*x3)*(y2-y4)-(y1-y3)*(x2*y4-x4*y2))/((x1-x3)*(y2-y4)-(y1-y3)*(x2-x4))
  c(px,py)
}
pxy<-matrix(ncol=2,nrow=nrow(eg))
t1 <- t2 <- rep(NA,length=nrow(eg))
for(i in 1:nrow(eg)){
  pxy[i,]<-intersection(input[eg[i,1],1],input[eg[i,1],2],input[eg[i,1],4],input[eg[i,1],5],
                        input[eg[i,2],1],input[eg[i,2],2],input[eg[i,2],4],input[eg[i,2],5])
  t1[i] <- (pxy[i,1]-input[eg[i,1],1])/input[eg[i,1],4]
  t2[i] <- (pxy[i,1]-input[eg[i,2],1])/input[eg[i,2],4]
}
sum(pxy[,1]>=window[1]&
    window[2]>=pxy[,1]&
    pxy[,2]>=window[1]&
    window[2]>=pxy[,2]&
    t1>=0&
    t2>=0)
#12015
# 
# mine <- eg[(pxy[,1]-window[1])>=0&
#      (window[2]-pxy[,1])>=0&
#      (pxy[,2]-window[1])>=0&
#      (window[2]-pxy[,2])>=0&
#      t1>=0&
#      t2>=0,]
# other <- read.table("test.txt",sep=" ")
# w <- c()
# for(i in 1:nrow(other)){
#   if(!any(other[i,1]==mine[,1]&other[i,2]==mine[,2])) w <- c(w,i)
# }




### x1+t*vx1=x+t*vx -> x+t*(vx-vx1)-x1=0
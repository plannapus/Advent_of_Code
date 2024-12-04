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

#x, y, z starting positions
#vx, vy, vz starting velocities
#t, u, v three collisions

#Eqs:
# x + t*vx = input[1,1]+t*input[1,4]
# y + t*vy = input[1,2]+t*input[1,5]
# z + t*vz = input[1,3]+t*input[1,6]
# x + u*vx = input[2,1]+u*input[2,4]
# y + u*vy = input[2,2]+u*input[2,5]
# z + u*vz = input[2,3]+u*input[2,6]
# x + v*vx = input[3,1]+v*input[3,4]
# y + v*vy = input[3,2]+v*input[3,5]
# z + v*vz = input[3,3]+v*input[3,6]
#
# t = (input[1,1]-x)/(vx-input[1,4])
# y*vx - x*dy = input[1,1]*input[1,5]+input[1,2]*input[1,4]+input[1,2]*dx-input[1,1]*dy-x*input[1,5]
# (input[2,5]-input[1,5])*x + (input[1,4]-input[2,4])*y + (input[1,2]-input[2,2])*vx + (input[2,1]-input[1,1])*vy = input[2,1]*input[2,5]-input[2,2]*input[2,4]-input[1,1]*input[1,5]+input[1,2]*input[1,4]
# ^ replicated 4 times since 4 variables
# then same 2 times additionally for Z:
# (input[2,6]-input[1,6])*x + (input[1,4]-input[2,4])*z + (input[1,3]-input[2,3])*vx + (input[2,1]-input[1,1])*vz = input[2,1]*input[2,6]-input[2,3]*input[2,4]-input[1,1]*input[1,6]+input[1,3]*input[1,4]

a = cbind(c(input[2,5]-input[1,5], #X coefficients
            input[4,5]-input[3,5],
            input[6,5]-input[5,5],
            input[8,5]-input[7,5],
            input[10,6]-input[9,6], #for Z computations
            input[12,6]-input[11,6]
            ),
          c(input[1,4]-input[2,4], #Y coefficients
            input[3,4]-input[4,4],
            input[5,4]-input[6,4],
            input[7,4]-input[8,4],
            0,0
            ),
          c(input[1,2]-input[2,2],
            input[3,2]-input[4,2],
            input[5,2]-input[6,2],
            input[7,2]-input[8,2],
            input[9,3]-input[10,3],
            input[11,3]-input[12,3]
            ),
          c(input[2,1]-input[1,1],
            input[4,1]-input[3,1],
            input[6,1]-input[5,1],
            input[8,1]-input[7,1],
            0,0
            ),
          c(0,0,0,0, #Z coefficients
            input[9,4]-input[10,4],
            input[11,4]-input[12,4]),
          c(0,0,0,0,
            input[10,1]-input[9,1],
            input[12,1]-input[11,1]))
b = cbind(c(input[2,1]*input[2,5]-input[2,2]*input[2,4]-input[1,1]*input[1,5]+input[1,2]*input[1,4],
            input[4,1]*input[4,5]-input[4,2]*input[4,4]-input[3,1]*input[3,5]+input[3,2]*input[3,4],
            input[6,1]*input[6,5]-input[6,2]*input[6,4]-input[5,1]*input[5,5]+input[5,2]*input[5,4],
            input[8,1]*input[8,5]-input[8,2]*input[8,4]-input[7,1]*input[7,5]+input[7,2]*input[7,4],
            input[10,1]*input[10,6]-input[10,3]*input[10,4]-input[9,1]*input[9,6]+input[9,3]*input[9,4],
            input[12,1]*input[12,6]-input[12,3]*input[12,4]-input[11,1]*input[11,6]+input[11,3]*input[11,4]))

r <- round(solve(a,b)) #Needs to be rounded because of floating point
sum(r[c(1:2,5)])
#1016365642179116
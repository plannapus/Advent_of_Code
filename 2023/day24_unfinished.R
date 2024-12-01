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


# #With input 27 and 192
# a1=(as.integer64(171541255868436)*(-134)-as.integer64(245300219115653)*(-204))
# b1=as.integer64(245300219115653)-as.integer64(171541255868436)
# c1= as.integer64(439565997787933)-as.integer64(271330638294580)
# #y = (a+b*dx+c*(92-dy))/69
# #y = (a+92*c)/69 + (b/69)*dx - dy*c/69
# #69*y-b1*dx+c1*dy = -(a1+92*c1)
# 
# #With input 75 and 183
# a2=(as.integer64(228412549072948)*(-20)-as.integer64(233729076694908)*(-50))
# b2=as.integer64(233729076694908)-as.integer64(228412549072948)
# c2= as.integer64(214942250915014)-as.integer64(146025596458637)
# #y = (a+b*dx+c*(35-dy))/-30
# #-30*y-b2*dx+c2*dy = -(a2+35*c2)
# 
# #With input 152 and 254
# a3=(as.integer64(248974332954192)*(29)-as.integer64(268129947037833)*(32))
# b3=as.integer64(268129947037833)-as.integer64(248974332954192)
# c3= as.integer64(437521748023833)-as.integer64(107603862021516)
# #3*y-b3*dx+c3*dy = -(a3+16*c3)
# 
# A <- cbind(c(69,-30,3),as.numeric(-1*c(b1,b2,b3)),as.numeric(c(c1,c2,c3)))
# B <- cbind(as.numeric(c(-(a1+92*c1),-(a2+35*c2),-(a3+16*c3))))
# Sy <-solve(A,B)
# y = as.integer64(Sy[1,1])
# dx = as.integer64(Sy[2,1])
# dy = as.integer64(Sy[3,1])
# 
# #With Input 1:
# x = as.integer64(260252047346974)+(y-as.integer64(360095837456982))*(66-dx)/(-174-dy)
# 
# a1 = (x-as.integer64(260252047346974))/(66-dx)
# #(512*a1+as.integer64(9086018216578))-dz*a1-z=0
# #z+a1*dz=-(512*a1+as.integer64(9086018216578))
# b1 = -(512*a1+as.integer64(9086018216578))
# 
# #with input 2:
# a2 = (x-as.integer64(511477129668052))/(-386-dx)
# #(512*a1+as.integer64(9086018216578))-dz*a1-z=0
# #z+a*dz=-(-322*a2+as.integer64(520727565082156))
# b2 = -(-322*a2+as.integer64(520727565082156))
# A <- cbind(c(1,1),as.numeric(c(a1,a2)))
# B <- cbind(as.numeric(c(b1,b2)))
# Sz <-solve(A,B)
# z = as.integer64(Sz[1,1])
# as.numeric(x+y+z)

#1016365642173836
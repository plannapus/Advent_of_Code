x <- 1
t <- 1
input <- readLines("input10.txt")
k<-1
cycles <- matrix(c(t,x),ncol=2)
while(k<=length(input)){
  if(input[k]=="noop"){
    t <- t+1
    k <- k+1
  }else{
    t <- t+2
    x <- x + as.integer(gsub("addx ","",input[k]))
    k <- k+1
  }
  cycles <- rbind(cycles, c(t,x))
}
values_at_checkpoints <- approx(cycles[,1],cycles[,2],c(20,60,100,140,180,220),method="constant",f=0)$y
sum(values_at_checkpoints*c(20,60,100,140,180,220))
#14040
fcycles <- cbind(1:240,approx(cycles[,1],cycles[,2],1:240,method="constant",f=0)$y)
crt <- matrix(0,nrow=40,ncol=6)
for(i in 1:240){
  sprite <- (fcycles[i,2]+(-1:1))%%40
  pos <- (i-1)%%40
  if(pos%in%sprite) crt[i]<-1
}
pdf("plot_day10.pdf",h=3,w=10)
image(1:40,1:6,crt,ylim=c(6,1),ann=F,ax=F)
dev.off()
#ZGCJZJFL
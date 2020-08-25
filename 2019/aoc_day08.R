#Day 8 Puzzle 1
input8=readLines("~/Desktop/input8.txt")
im=array(as.integer(el(strsplit(input8,""))),dim=c(25,6,nchar(input8)/25/6))
l0=im[,,which.min(apply(im,3,function(x)sum(x==0)))]
sum(l0==1)*sum(l0==2)
#1463

#Day 8 Puzzle 2
m=matrix(nr=6,nc=25)
for(i in 1:25){
  for(j in 1:6){
    sb=im[i,j,]
    m[j,i]=head(sb[sb!=2],1)
  }
}
image(1:25,1:6,t(m)[,6:1],asp=1,col=c("white","black"),ann=F,ax=F)
#GKCKH

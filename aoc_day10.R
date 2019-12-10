#Day 10 Puzzle 1
inp = readLines("input10.txt")
map = do.call(rbind,strsplit(inp,""))
coord = which(map=="#",arr.ind=TRUE)
m = matrix(nr=nrow(coord),nc=nrow(coord))
ast = vector(length=nrow(coord))
for(i in 1:nrow(coord)){
  angs = apply(coord,1,function(x){d=coord[i,]-x;atan2(d[1],d[2])})
  ast[i] = length(unique(angs[-i]))
}
max(ast)
#344

#Day 10 Puzzle 2
station = coord[which.max(ast),]-1
asteroids = coord[-which.max(ast),]-1
n=0
angs = apply(asteroids,1,function(x){d=station-x;atan2(d[1],d[2])})
topped = angs + pi/2
topped[topped >= pi] = topped[topped >= pi] - 2*pi
angs_sort = unique(angs[order(topped)])
l = apply(asteroids,1,function(x)sum((station-x)^2))
destroyed = c()
for(i in seq_along(angs_sort)){
  wang = angs_sort[i]
  wlos = angs==wang
  wlen = min(l[wlos])
  n = n+1
  if(n==200){ast200 = asteroids[wlos & l==wlen,]}
  destroyed = c(destroyed,which(wlos & l==wlen))
}
asteroids = asteroids[-destroyed,]
ast200[2]*100 + ast200[1]
#2732

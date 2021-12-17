step <- function(x0,y0,xv,yv){
  x1 <- x0 + xv
  y1 <- y0 + yv
  if(xv < 0) xv <- xv + 1
  if(xv > 0) xv <- xv - 1
  yv <- yv - 1
  c(x1,y1,xv,yv)
}
is_target <- function(x,y){
  x %in% 85:145 & y %in% -163:-108
}
max_height <- function(xv,yv){
  Y <- c(0,0)
  x0 <- y0 <- 0
  while(Y[length(Y)]>=Y[length(Y)-1]){
    X <- step(x0,y0,xv,yv)
    x0 <- X[1]
    y0 <- X[2]
    Y <- c(Y,y0)
    xv <- X[3]
    yv <- X[4]
  }
  max(Y)
}
test <- expand.grid(1:200,1:200)
it <- rep(FALSE,nrow(test))
for(i in 1:nrow(test)){
    x0 <- y0 <- 0
    xv <- test[i,1]
    yv <- test[i,2]
    while(y0 > -163){
      X <- step(x0,y0,xv,yv)
      x0 <- X[1]
      y0 <- X[2]
      xv <- X[3]
      yv <- X[4]
      if(is_target(x0,y0)) break
    }
    it[i] <- is_target(x0,y0)
    if(!i%%100)cat(i,"\r")
}
on_target <- test[it,]
max(apply(on_target,1,function(x)max_height(x[1],x[2])))
#13203

test <- expand.grid(1:200,-200:200)
it <- rep(FALSE,nrow(test))
for(i in 1:nrow(test)){
  x0 <- y0 <- 0
  xv <- test[i,1]
  yv <- test[i,2]
  while(y0 > -163){
    X <- step(x0,y0,xv,yv)
    x0 <- X[1]
    y0 <- X[2]
    xv <- X[3]
    yv <- X[4]
    if(is_target(x0,y0)) break
  }
  it[i] <- is_target(x0,y0)
  if(!i%%100)cat(i,"\r")
}
sum(it)
#5644
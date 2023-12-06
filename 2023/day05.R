options(digits=22)
input <- readLines(read.input(5))
#input <- readLines("test05.txt")
seeds <- strsplit(gsub("seeds: ","",input[1])," ")[[1]]
seeds <- as.numeric(seeds)
w <- which(input=="")
soil <- read.table(textConnection(input[(w[1]+2):(w[2]-1)]),sep=" ",header=FALSE)
fert <- read.table(textConnection(input[(w[2]+2):(w[3]-1)]),sep=" ",header=FALSE)
water <- read.table(textConnection(input[(w[3]+2):(w[4]-1)]),sep=" ",header=FALSE)
light <- read.table(textConnection(input[(w[4]+2):(w[5]-1)]),sep=" ",header=FALSE)
temp <- read.table(textConnection(input[(w[5]+2):(w[6]-1)]),sep=" ",header=FALSE)
hum <- read.table(textConnection(input[(w[6]+2):(w[7]-1)]),sep=" ",header=FALSE)
loc <- read.table(textConnection(input[(w[7]+2):length(input)]),sep=" ",header=FALSE)
so <- fe <- wa <- li <- te <- hu <- locs <- c()
for(i in seq_along(seeds)){
  s <- seeds[i]
  for(j in 1:nrow(soil)){
    if(s>=soil[j,2] & s<=soil[j,2]+soil[j,3]-1){
      s <- approx(c(soil[j,2],soil[j,2]+soil[j,3]-1),
                  c(soil[j,1],soil[j,1]+soil[j,3]-1),
                  s)$y
      break
    }
  }
  so[i] <- s
  for(j in 1:nrow(fert)){
    if(s>=fert[j,2] & s<=fert[j,2]+fert[j,3]-1){
      s <- approx(c(fert[j,2],fert[j,2]+fert[j,3]-1),
                  c(fert[j,1],fert[j,1]+fert[j,3]-1),
                  s)$y
      break
    }
  }
  fe[i] <- s
  for(j in 1:nrow(water)){
    if(s>=water[j,2] & s<=water[j,2]+water[j,3]-1){
      s <- approx(c(water[j,2],water[j,2]+water[j,3]-1),
                  c(water[j,1],water[j,1]+water[j,3]-1),
                  s)$y
      break
    }
  }
  wa[i] <- s
  for(j in 1:nrow(light)){
    if(s>=light[j,2] & s<=light[j,2]+light[j,3]-1){
      s <- approx(c(light[j,2],light[j,2]+light[j,3]-1),
                  c(light[j,1],light[j,1]+light[j,3]-1),
                  s)$y
      break
    }
  }
  li[i] <- s
  for(j in 1:nrow(temp)){
    if(s>=temp[j,2] & s<=temp[j,2]+temp[j,3]-1){
      s <- approx(c(temp[j,2],temp[j,2]+temp[j,3]-1),
                  c(temp[j,1],temp[j,1]+temp[j,3]-1),
                  s)$y
      break
    }
  }
  te[i] <- s
  for(j in 1:nrow(hum)){
    if(s>=hum[j,2] & s<=hum[j,2]+hum[j,3]-1){
      s <- approx(c(hum[j,2],hum[j,2]+hum[j,3]-1),
                  c(hum[j,1],hum[j,1]+hum[j,3]-1),
                  s)$y
      break
    }
  }
  hu[i] <- s
  for(j in 1:nrow(loc)){
    if(s>=loc[j,2] & s<=loc[j,2]+loc[j,3]-1){
      s <- approx(c(loc[j,2],loc[j,2]+loc[j,3]-1),
                  c(loc[j,1],loc[j,1]+loc[j,3]-1),
                  s)$y
      break
    }
  }
  locs[i]<-s
}
min(locs)
#196167384

seeds2 <- matrix(seeds,ncol=2,byrow=TRUE)
ranges <- cbind(seeds2[,1],seeds2[,1]+seeds2[,2]-1)

#srRe X
#rsRe X
#sreR X
#rRse 
#serR

f <- function(r1,r2,z){
  nr <- rbind(c(r1,r2))
  for(i in 1:nrow(z)){
    start <- z[i,2]
    end <- z[i,2]+z[i,3]-1
    if(end<r2&end>r1){
      a <- rbind(c(r1,end),c(end+1,r2))
      nr <- do.call(rbind,apply(a,1,\(x)f(x[1],x[2],z), simplify=FALSE))
      break
    }
    if(start<=r1 & end>=r2){
      nr <- round(approx(c(start,end),
             c(z[i,1],z[i,1]+z[i,3]-1),
             c(r1,r2))$y)
      break
    }
    if(start<r2&start>r1){
      a <- rbind(c(r1,start),c(start,r2))
      nr <- do.call(rbind,apply(a,1,\(x)f(x[1],x[2],z), simplify=FALSE))
      break
    }
  }
  matrix(nr,ncol=2)
}

m <- c()
for(i in 1:nrow(ranges)){
  range <- ranges[i,]
  so <- f(range[1],range[2],soil)
  fe <- do.call(rbind,apply(so,1,\(x)f(x[1],x[2],fert),simplify=FALSE))
  wa <- do.call(rbind,apply(fe,1,\(x)f(x[1],x[2],water),simplify=FALSE))
  li <- do.call(rbind,apply(wa,1,\(x)f(x[1],x[2],light),simplify=FALSE))
  te <- do.call(rbind,apply(li,1,\(x)f(x[1],x[2],temp),simplify=FALSE))
  hu <- do.call(rbind,apply(te,1,\(x)f(x[1],x[2],hum),simplify=FALSE))
  lo <- do.call(rbind,apply(hu,1,\(x)f(x[1],x[2],loc),simplify=FALSE))
  m[i] <- min(lo)
}
min(m)
#125742456

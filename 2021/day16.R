input <- el(strsplit(readLines("input16.txt"),""))

parse_input<-function(input)as.integer(el(strsplit(
  paste(sapply(input,
               function(x)switch(x,
                                 "0" = "0000",
                                 "1" = "0001",
                                 "2" = "0010",
                                 "3" = "0011",
                                 "4" = "0100",
                                 "5" = "0101",
                                 "6" = "0110",
                                 "7" = "0111",
                                 "8" = "1000",
                                 "9" = "1001",
                                 "A" = "1010",
                                 "B" = "1011",
                                 "C" = "1100",
                                 "D" = "1101",
                                 "E" = "1110",
                                 "F" = "1111")),collapse=""),"")))
b <- parse_input(input)
V=0
bit2int <- function(x)sum(x*2^(length(x):1-1))

f <- function(binary){
  version <- bit2int(binary[1:3])
  V <<- V+version
  binary <- binary[-(1:3)]
  type <- bit2int(binary[1:3])
  binary <- binary[-(1:3)]
  if(type==4){
    nb <- c()
    repeat{
      n <- binary[1:5]
      nb <- c(nb,bit2int(n[2:4]))
      binary <- binary[-(1:5)]
      if(n[1]==0) break
    }
  }else{
    ltype <- binary[1]
    binary <- binary[-1]
    if(ltype==0){
      l <- binary[1:15]
      binary <- binary[-(1:15)]
      l <- bit2int(l)
      subpack <- binary[1:l]
      while(length(subpack)){
        subpack <- f(subpack)$data
      }
      binary <- binary[-(1:l)]
    }else{
      l <- binary[1:11]
      binary <- binary[-(1:11)]
      nsub <- bit2int(l)
      for(i in 1:nsub){
        binary <- f(binary)$data
      }
    }
  }
  list(data=binary, version=version, V=V)
}

f(b)$V
#999

options(digits=22)
g <- function(binary,verbose=FALSE){
  version <- bit2int(binary[1:3])
  binary <- binary[-(1:3)]
  type <- bit2int(binary[1:3])
  binary <- binary[-(1:3)]
  if(type==4){
    nb <- c()
    repeat{
      n <- binary[1:5]
      nb <- c(nb,n[2:5])
      binary <- binary[-(1:5)]
      if(n[1]==0) break
    }
    nb <- bit2int(nb)
  }else{
    ltype <- binary[1]
    binary <- binary[-1]
    subvalue <- c()
    if(ltype==0){
      l <- binary[1:15]
      binary <- binary[-(1:15)]
      l <- bit2int(l)
      subpack <- binary[1:l]
      while(length(subpack)){
        subp <- g(subpack)
        subpack <- subp$data
        subvalue <- c(subvalue,subp$value)
      }
      binary <- binary[-(1:l)]
    }else{
      l <- binary[1:11]
      binary <- binary[-(1:11)]
      nsub <- bit2int(l)
      for(i in 1:nsub){
        subp <- g(binary)
        binary <- subp$data
        subvalue <- c(subvalue, subp$value)
      }
    }
    if(type==0){
      nb <- sum(subvalue)
    }else if(type==1){
      nb <- prod(subvalue)
    }else if(type==2){
      nb <- min(subvalue)
    }else if(type==3){
      nb <- max(subvalue)
    }else if(type==5){
      nb <- as.integer(subvalue[1]>subvalue[2])
    }else if(type==6){
      nb <- as.integer(subvalue[1]<subvalue[2])
    }else if(type==7){
      nb <- as.integer(subvalue[1]==subvalue[2])
    }
    
  }
  if(verbose){
    if(type!=4) cat(nb,"\tT=",type,"\tS=",subvalue,"\n")
    if(type==4) cat(nb,"\tT=",type,"\n")
  }
  list(data=binary, value=nb, type=type)
}
g(b)$value
#3408662834145

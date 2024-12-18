input <- readLines(read.input(17))
#input <- c("Register A: 729","Register B: 0","Register C: 0","","Program: 0,1,5,4,3,0")
A <- as.integer(regmatches(input[1],gregexpr("[0-9]+",input[1]))[[1]])
B <- as.integer(regmatches(input[2],gregexpr("[0-9]+",input[2]))[[1]])
C <- as.integer(regmatches(input[3],gregexpr("[0-9]+",input[3]))[[1]])
prog <- as.integer(el(strsplit(gsub("Program: ","",input[5]),",")))
i <- 1
combo <- \(x){
  if(x%in%0:3) return(x)
  if(x==4) return(A)
  if(x==5) return(B)
  if(x==6) return(C)
}
output <- c()
while(i<=length(prog)){
  if(prog[i]==0){
    A <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }else if(prog[i]==1){
    B <- bitXor(B,prog[i+1])
    i <- i+2
  }else if(prog[i]==2){
    B <- combo(prog[i+1])%%8
    i <- i+2
  }else if(prog[i]==3){
    if(A==0){
      i <- i+2
    }else{
      i <- prog[i+1]+1
    }
  }else if(prog[i]==4){
    B <- bitXor(B,C)
    i <- i+2
  }else if(prog[i]==5){
    output <- c(output,combo(prog[i+1])%%8)
    i <- i+2
  }else if(prog[i]==6){
    B <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }else if(prog[i]==7){
    C <- floor(A/(2^combo(prog[i+1])))
    i <- i+2
  }
  if(any(is.na(B))) stop()
}
cat(output,sep=",")
#7,1,2,3,2,6,7,2,5

options(digits=22)
opcode <- \(prog,A,B,C){
  combo <- \(x){
    if(x%in%0:3) return(x)
    if(x==4) return(A)
    if(x==5) return(B)
    if(x==6) return(C)
  }
  i <- 1
  output <- c()
  while(i<=length(prog)){
    if(prog[i]==0){
      A <- floor(A/(2^combo(prog[i+1])))
      i <- i+2
    }else if(prog[i]==1){
      B <- bitXor(B,prog[i+1])
      i <- i+2
    }else if(prog[i]==2){
      B <- combo(prog[i+1])%%8
      i <- i+2
    }else if(prog[i]==3){
      if(A==0){
        i <- i+2
      }else{
        i <- prog[i+1]+1
      }
    }else if(prog[i]==4){
      B <- bitXor(B,C)
      i <- i+2
    }else if(prog[i]==5){
      output <- c(output,combo(prog[i+1])%%8)
      i <- i+2
    }else if(prog[i]==6){
      B <- floor(A/(2^combo(prog[i+1])))
      i <- i+2
    }else if(prog[i]==7){
      C <- floor(A/(2^combo(prog[i+1])))
      i <- i+2
    }
  }
  output
  #return(paste(output,collapse=","))
}

a <- 0
repeat{ #damn... bitXor and %% don t work with >32 bits integer apparently...
  o <- opcode(prog,a,0,0)
  if(identical(o,prog)) break
  a <- a+1
  if(!a%%1000)cat(a,"\r")
}
a
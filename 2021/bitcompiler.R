parse_input<-function(input)as.integer(el(strsplit(
  paste(sapply(el(strsplit(input,"")),
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

bit2int <- function(x)sum(x*2^(length(x):1-1))

bitcompiler <- function(binary,verbose=FALSE){
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
        subp <- bitcompiler(subpack)
        subpack <- subp$data
        subvalue <- c(subvalue,subp$value)
      }
      binary <- binary[-(1:l)]
    }else{
      l <- binary[1:11]
      binary <- binary[-(1:11)]
      nsub <- bit2int(l)
      for(i in 1:nsub){
        subp <- bitcompiler(binary)
        binary <- subp$data
        subvalue <- c(subvalue, subp$value)
      }
    }
    nb <- switch(as.character(type),
                 '0'=sum(subvalue),
                 '1'=prod(subvalue),
                 '2'=min(subvalue),
                 '3'=max(subvalue),
                 '5'=as.integer(subvalue[1]>subvalue[2]),
                 '6'=as.integer(subvalue[1]<subvalue[2]),
                 '7'=as.integer(subvalue[1]==subvalue[2]))
  }
  if(verbose){
    if(type!=4) cat(nb,"\tT=",type,"\tS=",subvalue,"\n")
    if(type==4) cat(nb,"\tT=",type,"\n")
  }
  list(data=binary, value=nb)
}

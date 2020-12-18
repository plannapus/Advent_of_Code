##Part1
input <- readLines("input18.txt")
#input <- c("2 * 3 + (4 * 5)",
#           "5 + (8 * 3 + 9 + 3 * 4 * 3)",
#           "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
#           "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
input <- paste0("(",input,")")
while(!all(grepl("^\\d+$",input))){
  w<-gregexpr("\\(([^()]+)\\)",input)
  innermost <- regmatches(input,w)
  innermost <- lapply(innermost,function(x)gsub("[()]","",x))
  innermost <- lapply(innermost,function(x){
                        y <- strsplit(x," ")
                        for(i in seq_along(y)){
                          while(length(y[[i]])>1){
                            z <- eval(parse(text=paste(y[[i]][1:3],collapse="")))
                            y[[i]] <- c(z,y[[i]][-(1:3)])
                          }
                        }
                      y
                      })
  input <- sapply(seq_along(input),function(j){
    inp <- input[j]
    inn <- innermost[[j]]
    W <- w[[j]]
    l <- attr(W,"match.length")
    L <- W+l
    for(i in seq_along(inn)){
      inp <- paste0(substr(inp,1,W[i]-1),inn[i],substr(inp,L[i],nchar(inp)))
      W <- W - l[i] + nchar(inn[i])
      L <- L - l[i] + nchar(inn[i])
    }
    inp
  })
}
library(bit64)
sum(as.integer64(input))
#510009915468

##Part2
input <- readLines("input18.txt")
input <- paste0("(",input,")")
evaluate_unit <- function(x){
  if(length(x)){
    y <- strsplit(x," ")
    for(i in seq_along(y)){
      while(length(y[[i]])>1){
        z <- eval(parse(text=paste(y[[i]][1:3],collapse="")))
        y[[i]] <- c(z,y[[i]][-(1:3)])
      }
    }
    return(y)
  }else{
    return(NA)
  }
}
replace_unit <- function(j, input, innermost, w){
    inp <- input[j]
    inn <- innermost[[j]]
    W <- w[[j]]
    l <- attr(W,"match.length")
    L <- W+l
    for(i in seq_along(inn)){
      if(!is.na(inn[i])){
        inp <- paste0(substr(inp,1,W[i]-1),inn[i],substr(inp,L[i],nchar(inp)))
        W <- W - l[i] + nchar(inn[i])
        L <- L - l[i] + nchar(inn[i])
      }
    }
    inp<-gsub("\\((\\d+)\\)","\\1",inp)
    inp
}
while(!all(grepl("^\\d+$",input))){
  p <- gregexpr("[0-9][0-9 +]+[0-9]",input)
  additions <- regmatches(input,p)
  additions <- lapply(additions,evaluate_unit)
  input <- sapply(seq_along(input),function(j)replace_unit(j,input,additions,p))
  input <- gsub("\\(([0-9]+)\\)","\\1",input)
  w<-gregexpr("\\(([^()+]+)\\)",input)
  innermost <- regmatches(input,w)
  innermost <- lapply(innermost,function(x)gsub("[()]","",x))
  innermost <- lapply(innermost,evaluate_unit)
  input <- sapply(seq_along(input),function(j)replace_unit(j,input,innermost,w))
}
sum(as.integer64(input))
#321176691637769

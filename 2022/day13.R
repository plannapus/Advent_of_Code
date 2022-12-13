input <- readLines("input13.txt")
left <- input[seq(1,length(input),3)]
right <- input[seq(2,length(input),3)]

det_order <- function(x,y){ #Not mine
  if(length(x)==0 & length(y)==0){
    return(NA)
  }else if(length(x)==0){
      return(TRUE)
  }else if(length(y)==0){
      return(FALSE)
  }
  if(is.numeric(x[[1]]) & is.numeric(y[[1]])){
    if(x[[1]] < y[[1]]){
      return(TRUE)
    }else if(x[[1]] > y[[1]]){
      return(FALSE)
    }else{
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(det_order(x,y))
    }
  }
  if(is.list(x[[1]]) & is.list(y[[1]])){
    ord <- det_order(x[[1]],y[[1]])
    if(is.na(ord)){
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(det_order(x,y))
    }else{
      return(ord)
    }
  }
  if(is.numeric(x[[1]]) & is.list(y[[1]])){
    x[[1]] <- list(x[[1]])
    return(det_order(x,y))
  }
  if(is.list(x[[1]]) & is.numeric(y[[1]])){
    y[[1]] <- list(y[[1]])
    return(det_order(x,y))
  }
}

ordered <- rep(NA,length(left))
for(i in seq_along(left)){
  jl <- fromJSON(left[i],simplify=FALSE)
  jr <- fromJSON(right[i],simplify=FALSE)
  ordered[i] <- det_order(jl,jr)
}
sum(which(ordered))
#5003

all <- input[input!=""]
Y <- sapply(all,function(x)fromJSON(x,simplify=FALSE))
pos1 <- sum(sapply(Y,det_order,y=list(list(2))))+1
pos2 <- sum(sapply(Y,det_order,y=list(list(6))))+2
pos1*pos2
#20280
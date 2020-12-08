## Part 1
input <- read.table("input08.txt",sep=" ", colClasses=c("character","integer"))
visited <- rep(FALSE, nrow(input))
k <- 0
i <- 1
while(!visited[i]){
  visited[i] <- TRUE
  if(input[i,1]=="nop"){
    i <- i + 1
  }else if(input[i,1]=="jmp"){
    i <- i + input[i,2]
  }else if(input[i,1]=="acc"){
    k <- k + input[i,2]
    i <- i + 1
  }
}
k
#1832

# Part 2
m <- which(visited&input[,1]%in%c("nop","jmp"))
for(j in seq_along(m)){
  visited <- rep(FALSE, nrow(input))
  k <- 0
  i <- 1
  while(!visited[i]&i<=nrow(input)){
    visited[i] <- TRUE
    op <- input[i,1]
    if(i==m[j]){
      op <- ifelse(op=="nop", "jmp", "nop")
    }
    if(op=="nop"){
      i <- i + 1
    }else if(op=="jmp"){
      i <- i + input[i,2]
    }else if(op=="acc"){
      k <- k + input[i,2]
      i <- i + 1
    }
  }
  if(i>nrow(input))cat(k)
}
#662
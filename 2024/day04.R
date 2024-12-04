grid <- do.call(rbind,strsplit(readLines(read.input(4)),""))
horiz <- c(apply(grid,1,paste,collapse=""),apply(grid,1,\(x)paste(rev(x),collapse="")))
vert <- c(apply(grid,2,paste,collapse=""),apply(grid,2,\(x)paste(rev(x),collapse="")))
diag_up <- c()
diag_down <- c()
for(i in 1:nrow(grid)){
  x <- i
  y <- 1
  dd <- c()
  while(x<=nrow(grid)&y<=nrow(grid)){
    dd <- c(dd,grid[x,y])
    x <- x+1
    y <- y+1
  }
  x <- i
  y <- 1
  du <- c()
  while(x>=1&y<=ncol(grid)){
    du <- c(du,grid[x,y])
    x <- x-1
    y <- y+1
  }
  diag_up <- c(diag_up, paste(du,collapse=""),paste(rev(du),collapse=""))
  diag_down <- c(diag_down,paste(dd,collapse=""),paste(rev(dd),collapse=""))
}
for(i in 2:ncol(grid)){
  x <- 1
  y <- i
  dd <- c()
  while(x<=nrow(grid)&y<=nrow(grid)){
    dd <- c(dd,grid[x,y])
    x <- x+1
    y <- y+1
  }
  x <- nrow(grid)
  y <- i
  du <- c()
  while(x>=1&y<=ncol(grid)){
    du <- c(du,grid[x,y])
    x <- x-1
    y <- y+1
  }
  diag_up <- c(diag_up, paste(du,collapse=""),paste(rev(du),collapse=""))
  diag_down <- c(diag_down,paste(dd,collapse=""),paste(rev(dd),collapse=""))
}

all <- c(horiz,vert,diag_up,diag_down)
length(unlist(regmatches(all,gregexpr("XMAS",all))))
#2521

n <- 0
for(i in 1:(nrow(grid)-2)){
  for(j in 1:(nrow(grid)-2)){
    if(grid[i,j]=="S"){
      if(grid[i+1,j+1]=="A"&
         grid[i+2,j+2]=="M"){
        if((grid[i+2,j]=="M"&grid[i,j+2]=="S")|(grid[i+2,j]=="S"&grid[i,j+2]=="M")){
          n <- n+1
        }
      }
    }
    if(grid[i,j]=="M"){
      if(grid[i+1,j+1]=="A"&
         grid[i+2,j+2]=="S"){
        if((grid[i+2,j]=="M"&grid[i,j+2]=="S")|(grid[i+2,j]=="S"&grid[i,j+2]=="M")){
          n <- n+1
        }
      }
    }
  }
}
#1912
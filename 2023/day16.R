source("read.input.R")
input <- do.call(rbind,strsplit(readLines(read.input(16)),""))
#input <- do.call(rbind,strsplit(readLines("test/test16.txt"),""))
beams <- visited <- data.frame(x=1,y=0,dir="E")
# plot(NA,xlim=c(0,111),ylim=c(111,0),ax=F,ann=F)
# w <- rbind(which(input=="/",arr.ind=T),which(input=="\\",arr.ind=T))
# points(w[,2:1],col="red",pch=19,cex=0.5)
# s <- rbind(which(input=="-",arr.ind=T),which(input=="|",arr.ind=T))
# points(s[,2:1],col="cadetblue",pch=19,cex=0.5)
# rect(0,0,111,111)

while(nrow(beams)){
  ridof <- c()
  for(i in 1:nrow(beams)){
    if(sum(visited$x==beams$x[i]&
           visited$y==beams$y[i]&
           visited$dir==beams$dir[i])>1){
      ridof <- c(ridof,i)
    }else{
      if(beams$dir[i]=="E") new <- c(beams$x[i],beams$y[i]+1)
      if(beams$dir[i]=="W") new <- c(beams$x[i],beams$y[i]-1)
      if(beams$dir[i]=="N") new <- c(beams$x[i]-1,beams$y[i])
      if(beams$dir[i]=="S") new <- c(beams$x[i]+1,beams$y[i])
      if(new[1]%in%c(0,111)|new[2]%in%c(0,111)){
        ridof <- c(ridof,i)
      }else{
        if(any(beams$x==new[1]&beams$y==new[2]&beams$dir==beams$dir[i])){
          ridof <- c(ridof,i)
        }else{
          beams$x[i] <- new[1]
          beams$y[i] <- new[2]
          visited <- rbind(visited,data.frame(x=new[1],y=new[2],dir=beams$dir[i]))
          if(input[new[1],new[2]]=="\\"){
            if(beams$dir[i]=="E"){
              beams$dir[i] <- "S"
            }else if(beams$dir[i]=="W"){
              beams$dir[i] <- "N"
            }else if(beams$dir[i]=="N"){
              beams$dir[i] <- "W"
            }else if(beams$dir[i]=="S"){
              beams$dir[i] <- "E"
            }
          }else if(input[new[1],new[2]]=="/"){
            if(beams$dir[i]=="E"){
              beams$dir[i] <- "N"
            }else if(beams$dir[i]=="W"){
              beams$dir[i] <- "S"
            }else if(beams$dir[i]=="N"){
              beams$dir[i] <- "E"
            }else if(beams$dir[i]=="S"){
              beams$dir[i] <- "W"
            }
          }else if(input[new[1],new[2]]=="|" & beams$dir[i]%in%c("E","W")){
            beams$dir[i] <- "N"
            beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="S"))
          }else if(input[new[1],new[2]]=="-" & beams$dir[i]%in%c("N","S")){
            beams$dir[i] <- "E"
            beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="W"))
          }
        }
      }
    }
  }
  if(length(ridof)){
    beams <- beams[-ridof,,drop=FALSE]
  }
  #points(visited$y,visited$x,pch=19,cex=0.5)
  cat(nrow(visited)," - ",nrow(unique(visited[,1:2]))-1,"\r")
}
nrow(unique(visited[,1:2]))-1
#6921

init_beams <- rbind(data.frame(x=1:110,y=0,dir="E"),
                    data.frame(x=0,y=1:110,dir="S"),
                    data.frame(x=111,y=1:110,dir="N"),
                    data.frame(x=1:110,y=111,dir="W"))
energized <- vector("integer",length=nrow(init_beams))

for(j in 1:nrow(init_beams)){
  beams <- visited <- init_beams[j,]
  while(nrow(beams)){
    ridof <- c()
    for(i in 1:nrow(beams)){
      if(sum(visited$x==beams$x[i]&
             visited$y==beams$y[i]&
             visited$dir==beams$dir[i])>1){
        ridof <- c(ridof,i)
      }else{
        if(beams$dir[i]=="E") new <- c(beams$x[i],beams$y[i]+1)
        if(beams$dir[i]=="W") new <- c(beams$x[i],beams$y[i]-1)
        if(beams$dir[i]=="N") new <- c(beams$x[i]-1,beams$y[i])
        if(beams$dir[i]=="S") new <- c(beams$x[i]+1,beams$y[i])
        if(new[1]%in%c(0,111)|new[2]%in%c(0,111)){
          ridof <- c(ridof,i)
        }else{
          if(any(beams$x==new[1]&beams$y==new[2]&beams$dir==beams$dir[i])){
            ridof <- c(ridof,i)
          }else{
            beams$x[i] <- new[1]
            beams$y[i] <- new[2]
            visited <- rbind(visited,data.frame(x=new[1],y=new[2],dir=beams$dir[i]))
            if(input[new[1],new[2]]=="\\"){
              if(beams$dir[i]=="E"){
                beams$dir[i] <- "S"
              }else if(beams$dir[i]=="W"){
                beams$dir[i] <- "N"
              }else if(beams$dir[i]=="N"){
                beams$dir[i] <- "W"
              }else if(beams$dir[i]=="S"){
                beams$dir[i] <- "E"
              }
            }else if(input[new[1],new[2]]=="/"){
              if(beams$dir[i]=="E"){
                beams$dir[i] <- "N"
              }else if(beams$dir[i]=="W"){
                beams$dir[i] <- "S"
              }else if(beams$dir[i]=="N"){
                beams$dir[i] <- "E"
              }else if(beams$dir[i]=="S"){
                beams$dir[i] <- "W"
              }
            }else if(input[new[1],new[2]]=="|" & beams$dir[i]%in%c("E","W")){
              beams$dir[i] <- "N"
              beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="S"))
            }else if(input[new[1],new[2]]=="-" & beams$dir[i]%in%c("N","S")){
              beams$dir[i] <- "E"
              beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="W"))
            }
          }
        }
      }
    }
    if(length(ridof)){
      beams <- beams[-ridof,,drop=FALSE]
    }
    cat(j," - ",nrow(unique(visited[,1:2]))-1,"\r")
  }
  energized[j] <- nrow(unique(visited[,1:2]))-1
}
cat("\n")
cat(max(energized))
#7594

# library(doSNOW)
# ray <- function(i){
#     beams <- visited <- init_beams[i,]
#     while(nrow(beams)){
#       ridof <- c()
#       for(i in 1:nrow(beams)){
#         if(sum(visited$x==beams$x[i]&
#                visited$y==beams$y[i]&
#                visited$dir==beams$dir[i])>1){
#           ridof <- c(ridof,i)
#         }else{
#           if(beams$dir[i]=="E") new <- c(beams$x[i],beams$y[i]+1)
#           if(beams$dir[i]=="W") new <- c(beams$x[i],beams$y[i]-1)
#           if(beams$dir[i]=="N") new <- c(beams$x[i]-1,beams$y[i])
#           if(beams$dir[i]=="S") new <- c(beams$x[i]+1,beams$y[i])
#           if(new[1]%in%c(0,111)|new[2]%in%c(0,111)){
#             ridof <- c(ridof,i)
#           }else{
#             if(any(beams$x==new[1]&beams$y==new[2]&beams$dir==beams$dir[i])){
#               ridof <- c(ridof,i)
#             }else{
#               beams$x[i] <- new[1]
#               beams$y[i] <- new[2]
#               visited <- rbind(visited,data.frame(x=new[1],y=new[2],dir=beams$dir[i]))
#               if(input[new[1],new[2]]=="\\"){
#                 if(beams$dir[i]=="E"){
#                   beams$dir[i] <- "S"
#                 }else if(beams$dir[i]=="W"){
#                   beams$dir[i] <- "N"
#                 }else if(beams$dir[i]=="N"){
#                   beams$dir[i] <- "W"
#                 }else if(beams$dir[i]=="S"){
#                   beams$dir[i] <- "E"
#                 }
#               }else if(input[new[1],new[2]]=="/"){
#                 if(beams$dir[i]=="E"){
#                   beams$dir[i] <- "N"
#                 }else if(beams$dir[i]=="W"){
#                   beams$dir[i] <- "S"
#                 }else if(beams$dir[i]=="N"){
#                   beams$dir[i] <- "E"
#                 }else if(beams$dir[i]=="S"){
#                   beams$dir[i] <- "W"
#                 }
#               }else if(input[new[1],new[2]]=="|" & beams$dir[i]%in%c("E","W")){
#                 beams$dir[i] <- "N"
#                 beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="S"))
#               }else if(input[new[1],new[2]]=="-" & beams$dir[i]%in%c("N","S")){
#                 beams$dir[i] <- "E"
#                 beams <- rbind(beams,data.frame(x=new[1],y=new[2],dir="W"))
#               }
#             }
#           }
#         }
#       }
#       if(length(ridof)){
#         beams <- beams[-ridof,,drop=FALSE]
#       }
#       cat(j," - ",nrow(unique(visited[,1:2]))-1,"\r")
#     }
#     nrow(unique(visited[,1:2]))-1
# }
# 
# cl <- makeCluster(4)
# registerDoSNOW(cl)
# pb <- txtProgressBar(max = nrow(init_beams), style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# energized <- foreach(i=1:nrow(init_beams), .combine=c, .options.snow = opts) %dopar% ray(i)
# stopCluster(cl)

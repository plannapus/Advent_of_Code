input <- readLines("input08.txt")
io <- do.call(rbind,strsplit(input,"\\|"))
output <- strsplit(gsub("^ ","",io[,2])," ")
sum(sapply(output,function(x)sum(nchar(x)%in%c(2:4,7))))
# 367

# 1
#2 3
# 4
#5 6
# 7

outputs <- c()
for(i in 1:nrow(io)){
  displays <- as.list(rep(NA,7))
  nb <- strsplit(gsub("^ ","",io[i,])," ")
  nc <- sapply(nb,nchar)
  pooln <- unlist(nb)
  poolc <- unlist(nc)
  output <- rep(NA,length(nb[[2]]))
  no <- tail(poolc,length(output))
  co <- tail(pooln, length(output))
  if(any(poolc==2)){
    un <- unique(unlist(strsplit(pooln[which(poolc==2)],"")))
    displays[[3]] <- displays[[6]] <- un
  }
  if(any(poolc==3)){
    sept <- unique(unlist(strsplit(pooln[which(poolc==3)],"")))
    if(!is.na(displays[[3]][[1]])){
      displays[[1]] <- sept[!sept%in%un]
    }else{
      displays[[1]] <- displays[[3]] <- displays[[6]] <- un
    }
  }
  if(any(poolc==4)){
    quatre <- unique(unlist(strsplit(pooln[which(poolc==4)],"")))
    if(!is.na(displays[[3]][[1]])){
      displays[[2]] <- displays[[4]] <- quatre[!quatre%in%displays[[3]]]
    }else{
      displays[[2]] <- displays[[4]] <- displays[[3]] <- displays[[6]] <- quatre
    }
  }
  if(any(poolc==6)){
    s069 <- strsplit(pooln[which(poolc==6)],"")
    S09 <- sapply(s069,function(x)all(displays[[3]]%in%x))
    S69 <- sapply(s069,function(x)all(displays[[4]]%in%x))
    if(any(!S09)){
      six <- s069[!S09][[1]]
      displays[[3]] <- letters[1:7][!letters[1:7]%in%six]
      displays[[6]] <- displays[[6]][!displays[[6]]%in%displays[[3]]]
    }
    if(any(!S69)){
      zero <- s069[!S69][[1]]
      displays[[4]] <- letters[1:7][!letters[1:7]%in%zero]
      displays[[2]] <- displays[[2]][!displays[[2]]%in%displays[[4]]]
    }
  }
  output[no==2] <- 1
  output[no==3] <- 7
  output[no==4] <- 4
  output[no==7] <- 8
  if(length(displays[[3]])==1) output[no==6&!grepl(displays[[3]],co)] <- 6
  if(length(displays[[4]])==1) output[no==6&!grepl(displays[[4]],co)] <- 0
  output[no==6&!output%in%c(0,6)] <- 9
  if(length(displays[[3]])==1){
    output[no==5&!grepl(displays[[6]],co)] <- 2
    output[no==5&grepl(displays[[3]],co)&grepl(displays[[6]],co)] <- 3
    output[no==5&!grepl(displays[[3]],co)] <- 5
  }else{
    output[no==5 & sapply(co,function(x)sum(displays[[3]]%in%el(strsplit(x,""))))==2] <- 3
    if(length(displays[[2]])==1){
      output[no==5 & output!=3 & !grepl(displays[[2]],co)] <- 2
    }else{
      output[no==5 & output!=3 & sapply(co,function(x)sum(displays[[2]]%in%el(strsplit(x,""))))==1] <- 2
    }
    output[no==5 & !output%in%c(2,3)]<-5
  }
  outputs[i]<-paste(output,collapse="")
}
sum(as.integer(outputs))
# 974512

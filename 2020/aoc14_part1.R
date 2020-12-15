input <- c(13,16,0,12,15,1)
while(length(input)!=2020){
  last <- tail(input,1)
  if(last%in%head(input,-1)){
    age <- tail(diff(which(input==last)),1)
  }else{
    age <- 0
  }
  input <- c(input, age)
}
input[2020]
#319

# input <- c(13,16,0,12,15,1)
# df <- data.frame(n=input,age=seq_along(input))
# current <- length(input)
# last <- 0
# while(current!=30000000){
#   current <- current+1 
#   if(last%in%df$n){
#     previous <- df[df$n==last,]
#     df <- rbind(df[df$n!=last,],data.frame(n=last,age=current))
#     age <- current-previous$age
#     last <- age
#   }else{
#     df <- rbind(df,data.frame(n=last,age=current))
#     last <- 0
#   }
#   if(!current%%1000)cat(current,"\r")
# }
input <- el(strsplit(readLines("input06.txt"),""))
for (i in 4:length(input)){
  if(length(unique(input[(i-3):i]))>3) stop(i)
}
#Error: 1142

for (i in 14:length(input)){
  if(length(unique(input[(i-13):i]))>13) stop(i)
}
#Error: 2803

input <- el(strsplit("620080001611562C8802118E34",""))

b<-as.integer(el(strsplit(paste(sapply(input,function(x)switch(x,
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
V=0
f=function(binary){
  v=sum(binary[1:3]*2^(2:0))
  type=sum(binary[4:6]*2^(2:0))
  if(type==4){
    ll=length(binary)-6
    nb = split(binary[-(1:6)],rep(1:(ll%/%5),each=5))
    s=sapply(nb,function(x)x[1])
    nb = nb[1:head(which(s==0),1)]
    NB = sapply(nb,function(x)x[-1]*2^(3:0))
  }else{
    i=binary[7]
    if(i==1){
      l=sum(binary[7+(1:11)]*2^(10:0))
      packet=binary[18+(1:(11*l))]
      packets=split(packet,rep(1:l,each=11))
      v = v + sum(sapply(packets,f))
    }else{
      l=sum(binary[j+7+(1:15)]*2^(14:0))
      packets=binary[18+(1:l)]
    }
  }
  return(v)
}
f(b)

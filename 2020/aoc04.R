###Part 1
input <- readLines("input04.txt")
input[input==""]<-"\n"
passports <- gsub("^ | $","",scan(text=paste(input,collapse=" "),what="",sep="\n"))
pass <- list()
for(i in seq_along(passports)){
  step <- do.call(rbind,strsplit(scan(text=passports[i],what="",sep=" "),":"))
  pass[[i]]<-step[,2]
  names(pass[[i]])<-step[,1]
  }
sum(sapply(pass,function(x)all(c("byr","iyr","eyr","hgt","hcl","ecl","pid")%in%names(x))))
#247

##Part2
valids <- sapply(pass,function(x)all(c("byr","iyr","eyr","hgt","hcl","ecl","pid")%in%names(x)))
p_and_v <- sapply(pass[valids],function(x){
  hgt <- as.integer(gsub("[^0-9]+","",x['hgt']))
  as.integer(x['byr'])<=2002 & as.integer(x['byr'])>=1920 & 
  as.integer(x['iyr'])<=2020 & as.integer(x['iyr'])>=2010 &
  as.integer(x['eyr'])>=2020 & as.integer(x['eyr'])<=2030 &
  ifelse(is.na(hgt),FALSE,ifelse(grepl("cm",x['hgt']),hgt>=150&hgt<=193,hgt>=59&hgt<=76)) &
  grepl("^#[0-9a-f]{6}$",x['hcl']) & x['ecl']%in%c('amb','blu','brn','gry','grn','hzl','oth') &
  grepl("^[0-9]{9}$",x['pid'])
})
sum(p_and_v)
#145
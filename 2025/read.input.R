read.input <- function(day){
  require(httr)
  cookie <- readLines("session.txt",warn=FALSE)
  g <- GET(sprintf("http://adventofcode.com/2025/day/%s/input",day),
           set_cookies("session"=cookie))
  textConnection(content(g,as="text"))
}

read.map <- function(day){
  inp <- read.input(day)
  do.call(rbind,strsplit(readLines(inp),""))
}

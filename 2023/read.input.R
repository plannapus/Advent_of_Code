read.input <- function(day){
  require(httr)
  cookie <- readLines("session.txt",warn=FALSE)
  g <- GET(sprintf("http://adventofcode.com/2023/day/%s/input",day),
           set_cookies("session"=cookie))
  content(g,as="text")
}
read.input <- function(day){
  require(httr)
  cookie <- readLines("session.txt",warn=FALSE)
  g <- GET(sprintf("http://adventofcode.com/2024/day/%s/input",day),
           set_cookies("session"=cookie))
  textConnection(content(g,as="text"))
}

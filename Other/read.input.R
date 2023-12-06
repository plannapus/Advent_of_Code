read.input <- function(year,day){
  require(httr)
  cookie <- readLines("session.txt",warn=FALSE)
  g <- GET(sprintf("http://adventofcode.com/%s/day/%s/input",year,day),
           set_cookies("session"=cookie))
  textConnection(content(g,as="text"))
}

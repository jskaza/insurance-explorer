library(shiny)

plans <- read.csv("data/data.csv", header = T)

asymptote_point <- function(d, c, o) {
  return(d + (o - d) / c)
}

copay_eligible <- function(net, loc, c_p, c_s, c_u, c_e) {
  if
}
library(shiny)
library(dplyr)

plans <- read.csv("data/data.csv", header = T)

asymptote_point <- function(d, c, o) {
  return(d + (o - d) / c)
}

cost_fun <- function(x, d, c, o) {
  case_when((0 <= x) & (x < d) ~ x,
            (d <= d + c * (x - d)) &
              (d + c * (x - d) < o) ~ d + c * (x - d),
            TRUE ~ o)
}

you_pay <- function(expenses, d, c){
  case_when(
    as.numeric(expenses) <= as.numeric(d) ~ as.numeric(expenses),
    TRUE ~ as.numeric(d + c*(expenses-d))
  )
}
library(shiny)
library(dplyr)

plans <- read.csv("data/data.csv", header = T)

asymptote_point <- function(d, c, o) {
  return(d + (o - d) / c)
}

copay_eligible <- function(loc, c_p, c_s, c_u, c_e, c_v) {
  case_when(
    loc == "primary" & !is.na(c_p) ~ TRUE,
    loc == "specialist" & !is.na(c_s) ~ TRUE,
    loc == "urgent" & !is.na(c_u) ~ TRUE,
    loc == "er" & !is.na(c_e) ~ TRUE,
    loc == "preventative" & !is.na(c_v) ~ TRUE,
    TRUE ~ FALSE
  )
}

calc_copay <- function(loc, n, c_p, c_s, c_u, c_e, c_v) {
  n * case_when(
    loc == "primary" ~ c_p,
    loc == "specialist" ~ c_s,
    loc == "urgent" ~ c_u,
    loc == "er" ~ c_e,
    loc == "preventative" ~ c_v
  )
}

you_pay <- function(expenses, d, c, o){
  case_when(
    as.numeric(expenses) <= as.numeric(d) ~ as.numeric(expenses),
    as.numeric(d + c*(expenses-d)) < as.numeric(o) ~ as.numeric(d + c*(expenses-d)),
    TRUE ~ as.numeric(o)
  )
}
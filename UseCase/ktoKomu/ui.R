library(openxlsx)
load("allStatements.rda")
dat <- read.xlsx("przewania2.xlsx",1)

library(shiny)

shinyUI(fluidPage(

  fluidRow(
    column(4,
           selectInput("kto",
                       "Kto przerywa:",
                       c("-wszyscy-",sort(unique(dat$komu))),
                       "Imię Nazwisko")),
    column(2, uiOutput("ktoZdjecie")),
    column(2, uiOutput("komuZdjecie")),
    column(4,
           selectInput("komu",
                       "Komu przerywa:",
                       c("-wszyscy-",sort(unique(dat$komu))),
                       "Imię Nazwisko"))
  ),
  uiOutput("distPlot")
))

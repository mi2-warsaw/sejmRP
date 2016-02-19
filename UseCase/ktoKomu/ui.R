library(openxlsx)
load("allStatements.rda")
dat <- read.xlsx("przewania2.xlsx",1)

library(shiny)

shinyUI(fluidPage(

  fluidRow(
    column(6,
           selectInput("kto",
                       "Kto przerywa:",
                       c("---",sort(unique(dat$komu))),
                       "Imię Nazwisko")),
    column(6,
           selectInput("komu",
                       "Komu przerywa:",
                       c("---",sort(unique(dat$komu))),
                       "Imię Nazwisko"))
    
  ),
  uiOutput("distPlot")
))

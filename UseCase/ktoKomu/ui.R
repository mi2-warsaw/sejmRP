library(openxlsx)
load("allStatements.rda")
dat <- read.xlsx("przerwania.xlsx",1)

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Kto komu przerywa"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("komu",
                  "Komu przerywa:",
                  unique(dat$komu),
                  "Imię Nazwisko"),
      selectInput("kto",
                  "Kto przerywa:",
                  unique(dat$kto),
                  "Imię Nazwisko")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("distPlot")
    )
  )
))

load("allStatements.rda")
load("slownikSurnamesMPW.rda")

library(shiny)
library(dplyr)

findStatements <- function(kto, komu) {
  cat (Sys.time(),kto, komu, "\n", sep=";", file = "log.txt", append = TRUE)
  tmp <- allStatements 
  if (komu != "---")
    allStatements <- filter(allStatements, grepl(surname_name, pattern=komu)) 
  if (kto != "---")
    allStatements <- filter(allStatements, grepl(statement, pattern=kto)) 
  if (komu == "---" & kto == "---")
    allStatements <- allStatements[1:100,]
  allStatements
}


shinyServer(function(input, output) {

  output$distPlot <- renderUI({
    a <- findStatements(input$kto, input$komu)
    ss <- a$statement
    linki <- sapply(strsplit(a$id_statement, split = ".", fixed = TRUE), function(v) {
      paste0("http://www.sejm.gov.pl/Sejm8.nsf/wypowiedz.xsp?posiedzenie=",v[1],"&dzien=",v[2],"&wyp=",v[3],"&view=S")
    })
    ss <- sapply(strsplit(ss, split = input$kto), function(v) {
      if (length(v)>1) {
        
        return(paste0(v[1], paste0("<font color='red'>",input$kto,"</font>", v[-1],collapse="")))
      } else {
        return(v)
      }
    })
    
    HTML(paste("<a href='",linki,"'>",seq_along(a$surname_name),". <b>",a$surname_name," (<i>",a$titles_order_points,"</i>)</b><br>",ss, "</a>",collapse="<br><br>\n"))
    
  })

  output$ktoZdjecie <- renderUI({
    kto <- input$kto
    link <- paste0("http://mamprawowiedziec.pl/zdjecia/politycy/",slownikSurnamesMPW[kto,"id"],".jpg")
    link2 <- paste0("http://mamprawowiedziec.pl/strona/polityk/",slownikSurnamesMPW[kto,"id"],"/informacje")
    if (kto == "-wszyscy-") return(HTML("&nbsp;"))
    HTML(paste("<a href='",link2,"'>",
               "<img width='100' src='",link,"'/>",
               "</a>\n"))
    
  })
  
  output$komuZdjecie <- renderUI({
    kto <- input$komu
    link <- paste0("http://mamprawowiedziec.pl/zdjecia/politycy/",slownikSurnamesMPW[kto,"id"],".jpg")
    link2 <- paste0("http://mamprawowiedziec.pl/strona/polityk/",slownikSurnamesMPW[kto,"id"],"/informacje")
    if (kto == "-wszyscy-") return(HTML("&nbsp;"))
    HTML(paste("<a href='",link2,"'>",
               "<img width='100' src='",link,"'/>",
               "</a>\n"))
    
  })
  
})

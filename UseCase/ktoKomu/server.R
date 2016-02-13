load("allStatements.rda")

library(shiny)
library(dplyr)

findStatements <- function(kto, komu) {
  allStatements %>%
    filter(grepl(surname_name, pattern=komu)) %>%
    filter(grepl(statement, pattern=kto)) 
}


shinyServer(function(input, output) {

  output$distPlot <- renderUI({
    a <- findStatements(input$kto, input$komu)
    ss <- a$statement
    ss <- sapply(strsplit(ss, split = input$kto), function(v) {
      if (length(v)>1) {
        
        return(paste0(v[1], paste0("<font color='red'>",input$kto,"</font>", v[-1],collapse="")))
      } else {
        return(v)
      }
    })
    
    HTML(paste("<b>",a$surname_name," (<i>",a$titles_order_points,"</i>)</b><br>",ss, collapse="<br><br>\n"))
    
  })

})

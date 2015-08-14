#opis

votings_get_votings_table <- function(page){
  #stopifnoty
  
  #getting votings table
  votings_table <- readHTMLTable(page, encoding = "UTF-8", stringsAsFactors = FALSE)[[1]]
  
  return(votings_table)
}
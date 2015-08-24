#' Getting meetings' table
#'
#' Function \code{votings_get_meetings_table} gets meetings' table.
#'
#' @details
#' // to do
#'
#' @usage votings_get_meetings_table(page)
#'
#' @param page page with votings in polish diet: http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7
#'
#' @return data frame with three unnamed columns
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votings_get_meetings_table <- function(page){
  stopifnot(is.character(page))
  
  #getting meetings table
  meetings_table <- readHTMLTable(page, encoding = "UTF-8", stringsAsFactors = FALSE)[[1]]
  
  #filling first column where number of meeting is missing
  meeting_number <- meetings_table[1,1]
  for(i in seq_len(nrow(meetings_table))){
    if(meetings_table[i,1]!=""){
      meeting_number <- meetings_table[i,1]
    }
    meetings_table[i,1] <- meeting_number
  }
  
  return(meetings_table)
}
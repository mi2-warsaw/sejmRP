#opis

votings_get_meetings_table <- function(page){
  #stopifnoty
  
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
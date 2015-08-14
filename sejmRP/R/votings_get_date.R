#opis

votings_get_date <- function(page){
  #stopifnoty
  
  #getting date
  date <- html_nodes(html(page), "h1")
  date <- html_text(date)
  date <- unlist(stri_extract_all_regex(date,"[0-9]{2}-[0-9]{2}-[0-9]{4}"))
  date <- as.character(strptime(date,"%d-%m-%Y"))
  
  return(date)
}
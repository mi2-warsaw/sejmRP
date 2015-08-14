#opis

votings_get_meetings_links <- function(home_page,page){
  #stopifnoty
  
  #getting meetings links
  meetings_links <- html_nodes(html(page), "td.left")
  meetings_links <- html_nodes(meetings_links, "a")
  meetings_links <- unlist(html_attrs(meetings_links),use.names=FALSE)
  meetings_links <- paste0(home_page,meetings_links)
  
  return(meetings_links)
}
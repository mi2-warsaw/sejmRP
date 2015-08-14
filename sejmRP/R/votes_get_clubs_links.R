#opis

votes_get_clubs_links <- function(home_page,page){
  #stopifnoty
  
  #getting clubs
  results_page <- html(page)
  votes_info <- html_nodes(results_page, ".center .right")
  votes_clubs <- html_text(votes_info)
  
  #getting links
  votes_links <- html_nodes(votes_info, "a")
  votes_links <- unlist(html_attrs(votes_links),use.names=FALSE)
  votes_links <- paste0(home_page,votes_links)
  
  #creating data frame with data
  votes_clubs_links <- data.frame(club=votes_clubs, links=votes_links, stringsAsFactors = FALSE)
  
  return(votes_clubs_links)
}
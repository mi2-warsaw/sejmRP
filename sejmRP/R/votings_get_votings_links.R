#opis

votings_get_votings_links <- function(home_page,page){
  #stopifnoty
  
  #getting votings links
  votings_links <- html_nodes(html(page), ".bold a")
  votings_links <- unlist(html_attrs(votings_links), use.names=FALSE)
  
  #removing votings about positions
  correct <- !stri_detect_regex(votings_links,"glosowaniaL")
  
  votings_links <- votings_links[correct]
  votings_links <- paste0(home_page,votings_links)
  
  return(votings_links)
}

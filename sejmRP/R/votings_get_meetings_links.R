#' Getting meetings' links
#'
#' Function \code{votings_get_meetings_links} gets meetings' links.
#'
#' @details
#' // to do
#'
#' @usage votings_get_meetings_links(home_page,page)
#'
#' @param home_page main page of polish diet: http://www.sejm.gov.pl/Sejm7.nsf/
#' @param page page with votings in polish diet: http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7
#'
#' @return character vector
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votings_get_meetings_links <- function(home_page,page){
  stopifnot(is.character(home_page),is.character(page))
  
  #getting meetings links
  meetings_links <- html_nodes(html(page), "td.left")
  meetings_links <- html_nodes(meetings_links, "a")
  meetings_links <- unlist(html_attrs(meetings_links),use.names=FALSE)
  meetings_links <- paste0(home_page,meetings_links)
  
  return(meetings_links)
}
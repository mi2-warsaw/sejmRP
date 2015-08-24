#' Getting votings' links
#'
#' Function \code{votings_get_votings_links} gets votings' links.
#'
#' @details
#' // to do
#'
#' @usage votings_get_votings_links(home_page,page)
#'
#' @param home_page main page of polish diet: http://www.sejm.gov.pl/Sejm7.nsf/
#' @param page meeting's page
#'
#' @return character vector
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votings_get_votings_links <- function(home_page,page){
  stopifnot(is.character(home_page),is.character(page))
  
  #getting votings links
  votings_links <- html_nodes(html(page), ".bold a")
  votings_links <- unlist(html_attrs(votings_links), use.names=FALSE)
  
  #removing votings about positions
  correct <- !stri_detect_regex(votings_links,"glosowaniaL")
  
  votings_links <- votings_links[correct]
  votings_links <- paste0(home_page,votings_links)
  
  return(votings_links)
}

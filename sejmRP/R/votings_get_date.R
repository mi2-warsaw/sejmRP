#' Getting date of meeting
#'
#' Function \code{votings_get_date} gets date of meeting.
#'
#' @details
#' // to do
#'
#' @usage votings_get_date(page)
#'
#' @param page meeting's page
#'
#' @return date in format YYYY-MM-DD as character
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votings_get_date <- function(page){
  stopifnot(is.character(page))
  
  #getting date
  date <- html_nodes(html(page), "h1")
  date <- html_text(date)
  date <- unlist(stri_extract_all_regex(date,"[0-9]{2}-[0-9]{2}-[0-9]{4}"))
  date <- as.character(strptime(date,"%d-%m-%Y"))
  
  return(date)
}
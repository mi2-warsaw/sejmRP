#' Getting votings' table
#'
#' Function \code{votings_get_votings_table} gets votings' table.
#'
#' @details
#' // to do
#'
#' @usage votings_get_votings_table(page)
#'
#' @param page meeting's page
#'
#' @return data frame with three columns: Nr, Godzina (Time), Temat (Topic)
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votings_get_votings_table <- function(page){
  stopifnot(is.character(page))
  
  #getting votings table
  votings_table <- readHTMLTable(page, encoding = "UTF-8", stringsAsFactors = FALSE)[[1]]
  
  return(votings_table)
}
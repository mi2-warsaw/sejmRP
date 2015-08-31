#' Getting statements
#'
#' Function \code{statements_get_statement} gets statements.
#'
#' @details
#' Function \code{statements_get_statement} gets statements' content.
#'
#' @usage statements_get_statement(type)
#'
#' @param 
#'
#' @return statements' content
#'
#' @examples
#' \dontrun{
#' home_page <- "http://www.sejm.gov.pl/Sejm7.nsf/"
#' page <- "http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=15&dzien=1&wyp=008"
#' statements_get_statement(home_page,page)}
#'
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda, Tomasz Mikołajczyk
#'

statements_get_statement <- function(page){
  stopifnot(is.character(page))
  
  page <- html(page)
  page <- html_nodes(page,".stenogram p")
  
  #getting statement content
  statement <- html_text(page)
  statement <- stri_trim_both(statement)
  statement <- paste0(statement,collapse=" ")
  statement <- stri_replace_all_regex(statement,"'","")
  
  return(statement)
}
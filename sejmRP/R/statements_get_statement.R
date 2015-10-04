#' Getting statements
#'
#' Function \code{statements_get_statement} gets statement's content.
#' 
#' @details
#' Function \code{statements_get_statement} gets statement's content.
#' Example of page with deputy's statement: 
#' http://www.sejm.gov.pl/Sejm7.nsf/wypowiedz.xsp?posiedzenie=15&dzien=1&wyp=008
#'
#' @usage statements_get_statement(page)
#'
#' @param page deputy's statement's page
#' @param Nmax max number of tries (if there is a problem with connection)
#'
#' @return character vector
#'
#' @examples
#' \dontrun{
#' page <- paste0('http://www.sejm.gov.pl/Sejm7.nsf/',
#'                'wypowiedz.xsp?posiedzenie=15&dzien=1&wyp=008')
#' statements_get_statement(page)}
#'
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda, Tomasz Mikolajczyk
#'
#' @export
#'

statements_get_statement <- function(page, Nmax=20) {
    stopifnot(is.character(page))
    
    repeat({
      Nmax <- Nmax - 1
      pageH <- try(html(page), silent=TRUE)
      if (class(pageH) != "try-error")
        break()
      cat("No connection, ",page," trying again\n")
      Sys.sleep("60")
      if (Nmax < 0) {
        stop("No internet connection")
      }
    })
    page <- html_nodes(pageH, ".stenogram p")
    
    # getting statement content
    statement <- html_text(page)
    statement <- stri_trim_both(statement)
    statement <- paste0(statement, collapse = " ")
    statement <- stri_replace_all_regex(statement, "'", "")
    
    return(statement)
} 

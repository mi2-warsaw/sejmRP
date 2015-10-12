#' Matching deputies with their positions to theirs' ids
#'
#' Function \code{statements_match_deputies_ids} matches deputies from statements's
#' page to theirs' ids from \emph{deputies} table.
#'
#' @details
#' Function \code{statements_match_deputies_ids} matches deputies from statements's
#' page to theirs' ids from \emph{deputies} table. The result of this function is
#' a data frame with deputies' data and ids. Because of encoding issue
#' on Windows operation system, you need to select if you use Windows.
#'
#' @usage statements_match_deputies_ids(deputy,host='services.mini.pw.edu.pl',windows=TRUE)
#'
#' @param deputy name of database
#' @param host name of host; default: 'services.mini.pw.edu.pl'
#' @param windows information of used operation system; default: TRUE
#'
#' @return data frame with two columns: id_deputy, surname_name
#'
#' @examples
#' \dontrun{
#' statements <- get_statements_table()
#' statements_match_deputies_ids(statements[1000,2])
#' statements_match_deputies_ids(statements[2000,2])}
#'
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'
#' @export
#'

statements_match_deputies_ids <- function(deputy,
  host='services.mini.pw.edu.pl',windows=TRUE){
  stopifnot(is.character(deputy),is.character(host),
            is.logical(windows))

  #extracting all words from deputy
  words <- unlist(stri_extract_all_words(deputy))

  #extracting all words for every deputy in deputies table
  deputies <- get_deputies_table(host=host,windows=windows)
  deputies_words <- stri_extract_all_words(deputies[,2])

  #finding correct deputy
  for(i in seq_len(length(deputies_words))){
    inter <- intersect(deputies_words[[i]],words)
    if(length(inter)==length(deputies_words[[i]])){
      which_one <- i
      break
    }
  }

  #picking correct deputy
  deputy <- deputies[i,c(1,2)]

  return(deputy)
}

#testy

# library(sejmRP)
#
# statements<-get_statements_table(host="192.168.137.38")
# deputies <- get_deputies_table(host="192.168.137.38")
#
# a <- deputies[,2]
# b <- statements[,2]
#
# statements_match_deputies_ids(b[3000],host="192.168.137.38")

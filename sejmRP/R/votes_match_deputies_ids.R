#' Matching deputies to theirs' ids
#'
#' Function \code{votes_match_deputies_ids} matches deputies to theirs' ids.
#'
#' @details
#' // to do
#'
#' @usage votes_match_deputies_ids(dbname,user,password,host,page,windows=TRUE)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param page club's voting's results page
#' @param windows information of used operation system; default: TRUE
#' 
#' @return invisible NULL
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

votes_match_deputies_ids <- function(dbname,user,password,host,page,windows=TRUE){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.character(page),is.logical(windows))
  
  #getting all of deputies' ids
  deputies_whole_ids <- deputies_get_ids(dbname,user,password,host,windows)
  
  #getting votes' results
  votes_results <- votes_get_results(page)
  
  #getting deputies' (from voting) ids 
  deputies_ids <- unname(deputies_whole_ids[votes_results[,1]])
  
  #creating data frame with data
  votes_deputies_ids <- cbind(votes_results,id=deputies_ids)
  
  return(votes_deputies_ids)
}
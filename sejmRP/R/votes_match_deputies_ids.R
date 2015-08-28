#' Matching deputies to theirs' ids
#'
#' Function \code{votes_match_deputies_ids} matches deputies from voting's results
#' page to theirs' ids from \emph{deputies} table.
#'
#' @details
#' Function \code{votes_match_deputies_ids} matches deputies from voting's results
#' page to theirs' ids from \emph{deputies} table. The result of this function is
#' a data frame with deputies' data, ids and votes. Because of encoding issue
#' on Windows operation system, you need to select if you use Windows.
#' Example of page with voting's results of PO club: 
#' http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=37494&KodKlubu=PO
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
#' @return data frame with three columns: deputy, vote, id
#'
#' @examples
#' \dontrun{
#' page <- "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=37494&KodKlubu=PO"
#' votes_match_deputies_ids(dbname,user,password,host,page,TRUE)
#' votes_match_deputies_ids(dbname,user,password,host,page,FALSE)}
#' 
#' @note
#' All information is stored in PostgreSQL database.
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
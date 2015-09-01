#' Importing votings table from a database
#'
#' Function \code{get_votings_table} imports votings table from a database.
#' 
#' @usage get_votings_table(dbname = 'sejmrp', user = 'reader', password = 'qux94874', 
#' host = 'services.mini.pw.edu.pl', sorted_by_id = TRUE)
#'
#' @param dbname name of database; default: 'sejmrp'
#' @param user name of user; default: 'reader'
#' @param password password of database; default: 'qux94874'
#' @param host name of host; default: 'services.mini.pw.edu.pl'
#' @param sorted_by_id information if table should be sorted by id; default: TRUE
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' votings <- get_votings_table()
#' dim(votings)
#' # [1] 5969    6
#' names(votings)
#' # [1] "id_voting"    "nr_meeting"   "date_meeting" "nr_voting"    "topic_voting" "link_results"}
#' 
#' @note
#' Default parameters use privilages of 'reader'. It can only SELECT data from database.
#' 
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#' 
#' @export
#'

get_votings_table <- function(dbname = 'sejmrp', user = 'reader', password = 'qux94874', 
  host = 'services.mini.pw.edu.pl', sorted_by_id = TRUE){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host))
  
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  
  #reading table
  if(sorted_by_id){
  votings <- dbGetQuery(database_diet,"SELECT * FROM votings ORDER BY id_voting")
  }
  else{
  votings <- dbGetQuery(database_diet,"SELECT * FROM votings")
  }
  
  return(votings)
}

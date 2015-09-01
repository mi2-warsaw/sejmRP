#' Importing statements table from a database
#'
#' Function \code{get_statements_table} imports statements table from a database.
#' 
#' @usage get_statements_table(dbname = 'sejmrp', user = 'reader', password = 'qux94874', 
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
#' statements <- get_statements_table()
#' dim(statements)
#' # [1] 12629     3
#' names(statements)
#' # [1] "id_statement" "surname_name" "statement"}
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

get_statements_table <- function(dbname = 'sejmrp', user = 'reader', password = 'qux94874', 
  host = 'services.mini.pw.edu.pl', sorted_by_id = TRUE){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host))
  
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  
  #reading table
  if(sorted_by_id){
    statements <- dbGetQuery(database_diet,"SELECT * FROM statements ORDER BY id_statement")
  }
  else {
    statements <- dbGetQuery(database_diet,"SELECT * FROM statements")
  }
  
  
  return(statements)
}

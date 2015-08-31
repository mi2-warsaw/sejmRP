#' Retrieve all votes from a database
#'
#' Function \code{get_all_votes} reads all votes from a database.
#' Default parameters use privilages of 'reader'. It can only SELECT data from database.
#' Note that, due to data size (~150 MB) it may take few seconds / minutes to download all votes. 
#' 
#' @usage get_all_votes(dbname = 'sejmrp', user = 'reader', password = 'qux94874', host = 'services.mini.pw.edu.pl')
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#'
#' @return a data.frame with  NULL
#'
#' @examples
#' \dontrun{
#' all_votes <- get_all_votes()
#' dim(all_votes)
#' # [1] 2741899       8
#' object.size(all_votes)
#' # 144287808 bytes}
#'
#' @author Przemyslaw Biecek
#' 
#' @export
#'

get_all_votes <- function(dbname = 'sejmrp', user = 'reader', password = 'qux94874', host = 'services.mini.pw.edu.pl'){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host))
  
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_votes <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)

  # add information about new SELECT to the counter table
  dbSendQuery(database_votes, 
                       paste0("INSERT INTO counter (what, date) VALUES ('votes','",Sys.Date(),"')"))

  # read data
  votes <- dbGetQuery(database_votes, "SELECT d.surname_name, v.club, v.vote, vv.id_voting, vv.nr_meeting, vv.nr_voting, vv.date_meeting, vv.topic_voting
                      FROM votes v, votings vv, deputies d
                      WHERE v.id_voting = vv.id_voting AND d.id_deputy = v.id_deputy")
  
  suppressWarnings(dbDisconnect(database_votes))
  return(invisible(votes))
}

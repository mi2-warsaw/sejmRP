#' Retrieve all votes from a database
#'
#' Function \code{get_all_votes} reads all votes from a database. 
#' 
#' @usage get_all_votes(dbname = 'sejmrp', user = 'reader', password = 'qux94874',
#' host = 'services.mini.pw.edu.pl', windows = TRUE)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param windows information of used operation system; default: TRUE
#'
#' @return data frame with NULL
#'
#' @examples
#' \dontrun{
#' all_votes <- get_all_votes()
#' dim(all_votes)
#' # [1] 2741899       8
#' object.size(all_votes)
#' # 144287808 bytes}
#' 
#' @note
#' Default parameters use privilages of 'reader'. It can only SELECT data from database.
#' Note that, due to data size (~150 MB) it may take few seconds / minutes to download all votes.
#' 
#' All information is stored in PostgreSQL database.
#' 
#' @author Przemyslaw Biecek
#' 
#' @export
#'

get_all_votes <- function(dbname = 'sejmrp', user = 'reader', password = 'qux94874',
  host = 'services.mini.pw.edu.pl', windows = TRUE){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.logical(windows))
  
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)

  #add information about new SELECT to the counter table
  dbSendQuery(database_diet, 
                       paste0("INSERT INTO counter (what, date) VALUES ('all_votes','",Sys.Date(),"')"))

  #read data
  votes <- dbGetQuery(database_diet, "SELECT d.surname_name, v.club, v.vote, vv.id_voting, vv.nr_meeting, vv.nr_voting, vv.date_meeting, vv.topic_voting
                      FROM votes v, votings vv, deputies d
                      WHERE v.id_voting = vv.id_voting AND d.id_deputy = v.id_deputy")
  
  #encode for windows
  if(windows){
    votes[,1] <- iconv(votes[,1],from = "UTF-8", to = "Windows-1250")
    votes[,3] <- iconv(votes[,3],from = "UTF-8", to = "Windows-1250")
    votes[,8] <- iconv(votes[,8],from = "UTF-8", to = "Windows-1250")
  }
  
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(votes))
}

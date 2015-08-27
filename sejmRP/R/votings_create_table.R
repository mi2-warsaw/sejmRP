#' Creating table with votings
#'
#' Function \code{votings_create_table} creates a table with votings.
#' 
#' @usage votings_create_table(dbname,user,password,host,home_page,page)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param home_page main page of polish diet: http://www.sejm.gov.pl/Sejm7.nsf/
#' @param page page with votings in polish diet: http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' home_page <- "http://www.sejm.gov.pl/Sejm7.nsf/"
#' page <- "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7"
#' votings_create_table(dbname,user,password,host,home_page,page)}
#' 
#' @note
#' Use only this function for first time, when the \emph{votings} table
#' is empty. Then use \code{votings_update_table}.
#' 
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'

votings_create_table <- function(dbname,user,password,host,home_page,page){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.character(home_page),is.character(page))
  
  #getting meetings table with meetings' numbers
  meetings_table <- votings_get_meetings_table(page)
  
  #getting meetings links with votings
  meetings_links <- votings_get_meetings_links(home_page,page)
  
  #remembering first voting id
  id_voting <- 1
  
  for(i in rev(seq_len(length(meetings_links)))){
    #getting meetings date
    meetings_date <- votings_get_date(meetings_links[i])
    
    #getting votings table with votings' numbers and topics
    votings_table <- votings_get_votings_table(meetings_links[i])
    
    #getting votings links
    votings_links <- votings_get_votings_links(home_page,meetings_links[i])
    
    #putting this data frame to database
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
    for(j in rev(seq_len(length(votings_links)))){
      dbSendQuery(database_diet,paste0("INSERT INTO votings (id_voting, nr_meeting, date_meeting,",
               "nr_voting, topic_voting, link_results) VALUES (", id_voting , ",", meetings_table[i,1], 
               ",'", meetings_date, "',", votings_table[j,1], ",'", votings_table[j,3], "','", 
               votings_links[j], "')"))
      
      id_voting <- id_voting + 1
    }
    suppressWarnings(dbDisconnect(database_diet))
  }
  
  return(invisible(NULL))
}
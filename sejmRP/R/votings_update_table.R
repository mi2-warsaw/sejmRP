#' Updating table with votings
#'
#' Function \code{votings_update_table} updates table with votings.
#'
#' @usage votings_update_table(dbname, user, password, host,
#'   home_page='http://www.sejm.gov.pl/Sejm7.nsf/', page=
#'   'http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7',
#'   verbose=FALSE)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param home_page main page of polish diet: http://www.sejm.gov.pl/Sejm7.nsf/
#' @param page page with votings in polish diet: 
#' http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?
#' symbol=posglos&NrKadencji=7
#' @param verbose if TRUE then additional info will be printed
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' votings_update_table(dbname, user, password, host)}
#' 
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'
#' @export
#'

votings_update_table <- function(dbname, user, password, host, home_page = "http://www.sejm.gov.pl/Sejm7.nsf/", 
                                 page = "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=posglos&NrKadencji=7",
                                 verbose=FALSE) {
    stopifnot(is.character(dbname), is.character(user), is.character(password), is.character(host),
              is.character(home_page), is.character(page))
    
    # checking last nr_meeting, removing records with that number (if flag file doesn't exist) and checking last id_voting
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
    last_nr_meeting <- dbGetQuery(database_diet, "SELECT max(nr_meeting) FROM votings")
    last_nr_meeting <- as.integer(last_nr_meeting)
    if (!file.exists("sejmRP_votings_flag")) {
        ids_to_remove <- dbGetQuery(database_diet, paste0("SELECT id_voting FROM votings WHERE nr_meeting=", last_nr_meeting))
        ids_to_remove <- min(ids_to_remove)
        dbSendQuery(database_diet, paste0("DELETE FROM votes WHERE id_voting>=", ids_to_remove))
        dbSendQuery(database_diet, paste0("DELETE FROM votings WHERE nr_meeting=", last_nr_meeting))
    }
    last_id_voting <- dbGetQuery(database_diet, "SELECT max(id_voting) FROM votings")
    last_id_voting <- as.integer(last_id_voting)
    suppressWarnings(dbDisconnect(database_diet))
    
    # getting meetings table with meetings' numbers
    meetings_table <- votings_get_meetings_table(page)
    
    # checking if votings table is up-to-date
    if (meetings_table[1, 1] == last_nr_meeting & file.exists("sejmRP_votings_flag")) {
        return(invisible(NULL))
    }
    
    # getting meetings links with votings
    meetings_links <- votings_get_meetings_links(home_page, page)
    
    # choosing the place from where we start update table
    which_to_update <- which(as.numeric(meetings_table[, 1]) > last_nr_meeting)
    meetings_table <- meetings_table[which_to_update, ]
    meetings_links <- meetings_links[which_to_update]
    
    # remembering first new voting id
    id_voting <- last_id_voting + 1
    
    for (i in rev(seq_len(length(meetings_links)))) {
        # getting meetings date
        meetings_date <- votings_get_date(meetings_links[i])
        if (verbose) {
          cat("Downloading",meetings_links[i],"\n")
        }
        
        # getting votings table with votings' numbers and topics
        votings_table <- votings_get_votings_table(meetings_links[i])
        
        # getting votings links
        votings_links <- votings_get_votings_links(home_page, meetings_links[i])
        
        # putting this data frame to database
        drv <- dbDriver("PostgreSQL")
        database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
        for (j in rev(seq_len(length(votings_links)))) {
            dbSendQuery(database_diet, paste0("INSERT INTO votings (id_voting, nr_meeting, date_meeting,",
                                              "nr_voting, topic_voting, link_results) VALUES (", 
                                              id_voting, ",", meetings_table[i, 1], ",'", meetings_date, "',",
                                              votings_table[j, 1], ",'", votings_table[j, 3], "','", votings_links[j], "')"))
            
            id_voting <- id_voting + 1
        }
        suppressWarnings(dbDisconnect(database_diet))
    }
    
    # creating a flag file when updating votings table is finished
    file.create("sejmRP_votings_flag")
    
    return(invisible(NULL))
} 

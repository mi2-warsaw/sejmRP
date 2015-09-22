#' Updating table with deputies' statements
#'
#' Function \code{statements_update_table} updates a table with deputies' statements.
#'
#' @usage statements_update_table(dbname, user, password, host,
#'   home_page = 'http://www.sejm.gov.pl/Sejm7.nsf/')
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param home_page main page of polish diet: http://www.sejm.gov.pl/Sejm7.nsf/
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' statements_update_table(dbname, user, password, host)}
#'
#' @note 
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda, Tomasz Mikolajczyk
#'
#' @export
#'

statements_update_table <- function(dbname, user, password, host, home_page = "http://www.sejm.gov.pl/Sejm7.nsf/") {
    stopifnot(is.character(dbname), is.character(user), is.character(password), is.character(host), is.character(home_page))
    
    # checking last id of statements
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
    last_id <- dbGetQuery(database_diet, "SELECT SUBSTRING(id_statement, '[0-9]+\\.[0-9]+') FROM statements")
    last_id <- max(as.numeric(last_id[, 1]))
    ids_numbers <- unlist(stri_extract_all_regex(last_id, "[0-9]+"))
    dbSendQuery(database_diet, paste0("DELETE FROM statements WHERE id_statement SIMILAR TO '", ids_numbers[1], "\\.", ids_numbers[2], 
        "\\.[0-9]{3,4}'"))
    suppressWarnings(dbDisconnect(database_diet))
    
    nr_meeting <- as.numeric(ids_numbers[1])
    nr_day <- as.numeric(ids_numbers[2])
    repeat {
        repeat {
            # get statements links of first new day of a meeting
            page <- paste0(home_page, "wypowiedz.xsp?posiedzenie=", nr_meeting, "&dzien=", nr_day, "&wyp=0")
            stenogram <- html_nodes(html(page), ".stenogram")
            statements_links <- html_nodes(stenogram, "h2 a")
            
            # move to next day of meeting if empty page found
            if (length(statements_links) == 0) {
                break
            }
            
            # get date
            statements_date <- votings_get_date(page)
            
            # get deputies' names, statements and statements' ids
            statements_data <- statements_get_statements_data(statements_links, home_page)
            
            # get statements
            statements <- unlist(lapply(statements_data[, 2], function(elem) {
                statement <- statements_get_statement(elem)
            }))
            
            # put dataframe to database
            drv <- dbDriver("PostgreSQL")
            database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
            
            for (i in seq_len(length(statements))) {
                id <- paste0(nr_meeting, ".", nr_day, ".", statements_data[i, 3])
                dbSendQuery(database_diet, paste0("INSERT INTO statements (id_statement, surname_name, date_statement, statement)", 
                  "VALUES ('", id, "','", statements_data[i, 1], "','", statements_date, "','", statements[i], "')"))
            }
            
            suppressWarnings(dbDisconnect(database_diet))
            
            # next day of meeting
            nr_day <- nr_day + 1
        }
        # break if last meeting found
        if (nr_day == 1) {
            break
        }
        # next meeting
        nr_day <- 1
        nr_meeting <- nr_meeting + 1
    }
    
    return(invisible(NULL))
} 

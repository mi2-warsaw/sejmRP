#' Creating table with deputies
#'
#' Function \code{deputies_create_table} creates a table with deputies.
#'
#' @usage deputies_create_table(dbname, user, password, host)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' deputies_create_table(dbname, user, password, host)}
#'
#' @note
#' Use only this function for first time, when the \emph{deputies} table
#' is empty. Then use \code{deputies_update_table}.
#' 
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'
#' @export
#'

deputies_create_table <- function(dbname, user, password, host) {
    stopifnot(is.character(dbname), is.character(user), is.character(password), is.character(host))
    
    # getting data from page with active deputies
    deputies_active <- deputies_get_data("active")
    
    # getting data from page with inactive deputies
    deputies_inactive <- deputies_get_data("inactive")
    
    # merging ids, names and surnames of active and inactive deputies
    id_deputies <- c(deputies_active[, 1], deputies_inactive[, 1])
    deputies <- c(deputies_active[, 2], deputies_inactive[, 2])
    
    # putting this data frame to database
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
    n <- length(deputies)
    for (i in seq_len(n)) {
        dbSendQuery(database_diet, paste0("INSERT INTO deputies (id_deputy, surname_name) VALUES (", 
                                            "'", id_deputies[i], "','", deputies[i], "')"))
    }
    suppressWarnings(dbDisconnect(database_diet))
    return(invisible(NULL))
} 

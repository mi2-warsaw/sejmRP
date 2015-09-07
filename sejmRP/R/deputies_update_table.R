#' Updating table with deputies
#'
#' Function \code{deputies_update_table} updates a table with deputies.
#'
#' @usage deputies_update_table(dbname, user, password, host)
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
#' deputies_update_table(dbname, user, password, host)}
#'
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'
#' @export
#'

deputies_update_table <- function(dbname, user, password, host) {
    stopifnot(is.character(dbname), is.character(user), is.character(password), is.character(host))
    
    # checking last id of deputies
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)
    last_id <- dbGetQuery(database_diet, "SELECT max(id_deputy) FROM deputies")
    last_id <- as.character(last_id)
    suppressWarnings(dbDisconnect(database_diet))
    
    # adding new deputies to database
    deputies_add_new(dbname, user, password, host, "active", last_id)
    deputies_add_new(dbname, user, password, host, "inactive", last_id)
    
    return(invisible(NULL))
} 

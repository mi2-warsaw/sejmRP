#' Creating table with deputies
#'
#' Function \code{deputies_create_table} creates table with deputies.
#'
#' @details
#' // to do
#'
#' @usage deputies_create_table(dbname,user,password,host)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#'
#' @return invisible NULL
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

deputies_create_table <- function(dbname,user,password,host){

  #getting data from page with active deputies
  deputies_active <- deputies_get_data("active")

  #getting data from page with inactive deputies
  deputies_inactive <- deputies_get_data("inactive")

  #merging ids, names and surnames of active and inactive deputies
  id_deputies <- c(deputies_active[,1],deputies_inactive[,1])
  deputies <- c(deputies_active[,2],deputies_inactive[,2])

  #putting this data frame to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  n <- length(deputies)
  for(i in seq_len(n)){
    dbSendQuery(database_diet,paste0("INSERT INTO deputies (id_deputy, surname_name) VALUES (",
                                     "'",id_deputies[i],"','",deputies[i],"')"))
  }
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}
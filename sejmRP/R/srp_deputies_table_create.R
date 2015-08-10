#' Creating table with deputies
#'
#' Function \code{srp_deputies_table_create} creates table with deputies.
#'
#' @details
#' // to do
#'
#' @usage srp_deputies_table_create(dbname,user,password,host)
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

srp_deputies_table_create <- function(dbname,user,password,host){

  #getting data from page with active deputies
  deputies_active <- srp_deputies_data("active")

  #getting data from page with inactive deputies
  deputies_inactive <- srp_deputies_data("inactive")

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
#' Getting deputies' ids
#'
#' Function \code{deputies_get_ids} gets deputies' ids.
#'
#' @details
#' // to do
#'
#' @usage deputies_get_ids(dbname,user,password,host,windows)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param windows information of used operation system; default: TRUE
#' 
#' @return named character vector
#'
#' @examples
#' // to do
#'
#' @author Piotr Smuda
#'

deputies_get_ids <- function(dbname,user,password,host,windows=TRUE){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.logical(windows))
  
  #getting deputies' ids
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  deputies_table <- dbReadTable(database_diet,"deputies")
  deputies_ids <- deputies_table[,1]
  
  #calling deputies' ids with their names and surnames
  if(windows){
    names(deputies_ids) <- iconv(deputies_table[,2],from="UTF-8",to="Windows-1250")
  }
  else {
    names(deputies_ids) <- deputies_table[,2]
  }
  suppressWarnings(dbDisconnect(database_diet))
  
  return(deputies_ids)
}


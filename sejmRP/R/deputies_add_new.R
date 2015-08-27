#' Adding new deputies to table
#'
#' Function \code{deputies_add_new} adds new deputies to a table with deputies.
#'
#' @details
#' Function \code{deputies_add_new} adds new deputies to a table with deputies.
#' Also there is a choice between types of deputies, because on the page 
#' of Polish diet deputies are splitted into \emph{active} and \emph{inactive}.
#' In addition id of the last added deputy in \emph{deputies} table is needed.
#'
#' @usage deputies_add_new(dbname,user,password,host,type,id)
#'
#' @param dbname name of database
#' @param user name of user
#' @param password password of database
#' @param host name of host
#' @param type type of deputies which be add to table with deputies: active, inactive
#' @param id id of deputies from which we start add new deputies
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' deputies_add_new(dbname,user,password,host,"active",id)
#' deputies_add_new(dbname,user,password,host,"inactive",id)}
#' 
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#'

deputies_add_new <- function(dbname,user,password,host,type,id){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.character(type),type=="active"||type=="inactive",
            is.character(id),as.numeric(id)%%1==0)
  
  #getting data from page with deputies
  deputies <- deputies_get_data(type)
  
  #finding maximum id of deputies
  max_id <- max(deputies[,1])
  
  #if there are deputies with higher ids than in database we add them
  if(max_id>id){
    
    #finding deputies with higher ids
    which_new_deputies <- which(deputies[,1]>id)
    id_new_deputies <- deputies[which_new_deputies,1]
    new_deputies <- deputies[which_new_deputies,2]
    
    #adding new deputies to database
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
    n <- length(new_deputies)
    for(i in seq_len(n)){
      dbSendQuery(database_diet,paste0("INSERT INTO deputies (id_deputy, surname_name) VALUES ('",
                                       id_new_deputies[i],"','",new_deputies[i],"')"))
    }
    suppressWarnings(dbDisconnect(database_diet))
  }
  
  return(invisible(NULL))
}
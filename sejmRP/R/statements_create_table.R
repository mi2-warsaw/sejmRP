#' Creating table with deputies' statements.
#'
#' Function \code{statements_create_table} creates a table with deputies' statements.
#'
#' @usage statements_create_table(dbname,user,password,host)
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
#' statements_create_table(dbname,user,password,host)}
#'
#' @note
#' Use only this function for first time, when the \emph{deputies} table
#' is empty. Then use \code{deputies_update_table}.
#' 
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda, Tomasz Mikołajczyk
#'

statements_create_table <- function(dbname,user,password,host,home_page){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host),is.character(home_page))
   
  #set meeting and day number to 1
  nr_meeting <- 1
  nr_day <- 1
  repeat{
    repeat{

      #get statements links of first day of a meeting
      page <- html(paste0(home_page,"wypowiedz.xsp?posiedzenie=",nr_meeting,"&dzien=",nr_day,"&wyp=0"))
      page <- html_nodes(page,".stenogram")
      statements_links <- html_nodes(page, "h2 a")
      
      #move to next day of meeting if empty page found
      if(length(statements_links)==0){
        break
      }
      
      #get deputies' names, statements and statements' ids
      statements_data <- statements_get_statements_data(statements_links)
      
      #get statements
      statements <- unlist(lapply(statements_data[,2],function(elem){
        statement <- statements_get_statement(elem)
      }))
      
      #put dataframe to database
      drv <- dbDriver("PostgreSQL")
      database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
      
      for(i in seq_len(length(statements))){
        id <- paste0(nr_meeting,".",nr_day,".",statements_data[i,3])
        dbSendQuery(database_diet,paste0("INSERT INTO statements (id_statement, surname_name, statement)",
                                         "VALUES ('", id , "','", statements_data[i,1], "','", statements[i],"')"))
      }
      
      suppressWarnings(dbDisconnect(database_diet))
      
      #next day of meeting
      nr_day <- nr_day + 1 
    }
    #break if last meeting found 
    if(nr_day==1){
      break
    } 
    #next meeting
    nr_day <- 1
    nr_meeting <- nr_meeting + 1
  }
  
  return(invisible(NULL))
}
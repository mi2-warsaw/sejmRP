#' Creating database
#'
#' Function \code{create_database} creates a database with four empty
#' tables: deputies, votings, votes, statements.
#' 
#' @details
#' \preformatted{
#' Created tables:
#' 1. deputies with columns:
#'     1) id_deputy - deputy's id,
#'     2) surname_name - deputy's names and surnames,
#' 2. votings with columns:
#'     1) id_voting - voting's id,
#'     2) nr_meeting - meeting's number,
#'     3) date_meeting - meeting's date,
#'     4) nr_voting - voting's number,
#'     5) topic_voting - voting's topic,
#'     6) link_results - link with voting's results,
#' 3. votes with columns:
#'     1) id_vote - vote's id,
#'     2) id_deputy - deputy's id,
#'     3) id_voting - voting's id,
#'     4) vote - deputy's vote, one of: "Za","Przeciw",
#'               "Wstrzymal sie","Nieobecny",
#'     5) club - deputy's club,
#' 4. statements with columns:
#'     1) id_statement - statement's id, like: 
#'     (meeting's number).(voting's number).(statement's number),
#'     2) surname_name - author of statement,
#'     3) date_statement - statement's date,
#'     4) statement - content of statement.}
#' 
#' @usage create_database(dbname,user,password,host)
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
#' create_database(dbname,user,password,host)}
#' 
#' @note
#' All information is stored in PostgreSQL database.
#'
#' @author Piotr Smuda
#' 
#' @export
#'
#' @import RPostgreSQL rvest stringi
#' @importFrom DBI dbDriver
#' @importFrom XML readHTMLTable
#' 

create_database <- function(dbname,user,password,host){
  stopifnot(is.character(dbname),is.character(user),is.character(password),
            is.character(host))
  
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)

  #creating table with deputies data
  dbSendQuery(database_diet, "CREATE TABLE deputies (id_deputy varchar(4) NOT NULL PRIMARY KEY,
               surname_name varchar(50) NOT NULL,
               CONSTRAINT uq_surname_name UNIQUE (surname_name))")

  #creating table with voting data
  dbSendQuery(database_diet, "CREATE TABLE votings (id_voting int NOT NULL PRIMARY KEY,
               nr_meeting int NOT NULL, date_meeting date NOT NULL,
               nr_voting int NOT NULL, topic_voting text NOT NULL,
               link_results varchar(200))")

  #creating table with votes data
  dbSendQuery(database_diet, "CREATE TABLE votes (id_vote int NOT NULL PRIMARY KEY,
      id_deputy varchar(4) NOT NULL, id_voting int NOT NULL, vote varchar(20) NOT NULL,
      club varchar(50), FOREIGN KEY (id_deputy) REFERENCES deputies(id_deputy),
      FOREIGN KEY (id_voting) REFERENCES votings(id_voting))")

  #creating table with statements data
  dbSendQuery(database_diet, "CREATE TABLE statements (id_statement varchar(11) NOT NULL PRIMARY KEY,
            surname_name varchar(100) NOT NULL, date_statement date NOT NULL, statement text NOT NULL)")
  
  #creating table with counter data
  dbSendQuery(database_diet, "CREATE TABLE counter (id SERIAL PRIMARY KEY, what varchar(10) NOT NULL,
               date varchar(10) NOT NULL)")

  #disconnecting to database
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}
#' Creating database
#'
#' Function \code{create_database} creates database with three empty
#' tables: deputies, votings, votes.
#'
#' @details
#' // to do
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
#' // to do
#'
#' @author Piotr Smuda
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
#   dbSendQuery(database_diet, "CREATE TABLE statements (id_statement int NOT NULL PRIMARY KEY,
#               surname_name varchar(50) NOT NULL, nr_meeting int NOT NULL, day_meeting int NOT NULL,  
#               statement text NOT NULL, FOREIGN KEY (surname_name) REFERENCES deputies(surname_name),
#               FOREIGN KEY (nr_meeting) REFERENCES votings(nr_meeting))")

  #disconnecting to database
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}
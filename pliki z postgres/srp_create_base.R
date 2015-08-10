library("RPostgreSQL")

srp_create_database <- function(dbname,user,password,host){
  #connecting to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)

  #creating table with deputies data
  dbSendQuery(database_diet, "CREATE TABLE Deputies (id_deputy varchar(4) NOT NULL PRIMARY KEY,
               surname_name varchar(50) NOT NULL,
               CONSTRAINT uq_surname_name UNIQUE (surname_name))")

  #creating table with voting data
  dbSendQuery(database_diet, "CREATE TABLE Votings (id_voting int NOT NULL PRIMARY KEY,
               nr_meeting int NOT NULL, date_meeting date NOT NULL,
               nr_voting int NOT NULL, topic_voting text NOT NULL,
               link_results varchar(200))")

  #creating table with votes data
  dbSendQuery(database_diet, "CREATE TABLE Votes (id_vote int NOT NULL PRIMARY KEY,
      id_deputy varchar(4) NOT NULL, id_voting int NOT NULL, vote varchar(20) NOT NULL,
      club varchar(50), FOREIGN KEY (id_deputy) REFERENCES Deputies(id_deputy),
      FOREIGN KEY (id_voting) REFERENCES Votings(id_voting))")

  #disconnecting to database
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}

#tests
dbname<-"sejmrp"
user<-"sejmrp"
password<-"pQwZ99Kj65x"
host<-"services.mini.pw.edu.pl"

database_path<-"H:\\R\\staz sejm\\database_diet.sqlite"
srp_create_database(database_path)
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbListTables(database_diet)
dbReadTable(database_diet,"deputies")
dbReadTable(database_diet,"votings")
dbReadTable(database_diet,"votes")
dbSendQuery(database_diet,"INSERT INTO deputies VALUES ('001','Jan Kowalski')")
dbSendQuery(database_diet,"INSERT INTO deputies VALUES (2,'Jan Kowalski')") #error, ze nieunikalny
dbReadTable(database_diet,"deputies")
dbSendQuery(database_diet,"DELETE FROM deputies WHERE id_deputy='001'")
dbReadTable(database_diet,"deputies")
suppressWarnings(dbDisconnect(database_diet))

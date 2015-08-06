library("RSQLite")

srp_create_database <- function(database_path){
  #connecting to database
  database_diet <- dbConnect(SQLite(), dbname = database_path)
  
  #creating table with deputies data
  dbSendQuery(database_diet, "CREATE TABLE Deputies (id_deputy varchar(4) NOT NULL PRIMARY KEY,
               surname_name nvarchar(50) NOT NULL,
               CONSTRAINT uq_surname_name UNIQUE (surname_name))")
  
  #creating table with voting data
  dbSendQuery(database_diet, "CREATE TABLE Votings (id_voting int NOT NULL PRIMARY KEY,
               nr_meeting int NOT NULL, date_meeting date NOT NULL,
               nr_voting int NOT NULL, topic_voting ntext NOT NULL,
               link_results nvarchar(200))")
  
  #creating table with votes data
  dbSendQuery(database_diet, "CREATE TABLE Votes (id_vote int NOT NULL PRIMARY KEY,
      id_deputy varchar(4) NOT NULL, id_voting int NOT NULL, vote nvarchar(20) NOT NULL,
      club nvarchar(50), FOREIGN KEY (id_deputy) REFERENCES Deputies(id_deputy),
      FOREIGN KEY (id_voting) REFERENCES Votings(id_voting))")
  
  #disconnecting to database
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}

#tests
database_path<-"H:\\R\\staz sejm\\database_diet.sqlite"
srp_create_database(database_path)
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbListTables(database_diet)
dbReadTable(database_diet,"Deputies")
dbReadTable(database_diet,"Votings")
dbReadTable(database_diet,"Votes")
dbSendQuery(database_diet,"INSERT INTO Deputies VALUES (1,'Jan Kowalski')")
dbSendQuery(database_diet,"INSERT INTO Deputies VALUES (2,'Jan Kowalski')") #error, ze nieunikalny
dbReadTable(database_diet,"Deputies")
dbSendQuery(database_diet,"DELETE FROM Deputies WHERE id_deputy=1")
dbReadTable(database_diet,"Deputies")
suppressWarnings(dbDisconnect(database_diet))

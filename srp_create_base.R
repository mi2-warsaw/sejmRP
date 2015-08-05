library("RSQLite")

function srp_create_base(path){
   base_diet <- dbConnect(SQLite(), dbname = base_path)

   dbSendQuery(base_diet, "CREATE TABLE Deputies (id_deputy varchar(4) NOT NULL PRIMARY KEY,
               surname_name nvarchar(50) NOT NULL,
               CONSTRAINT uq_surname_name UNIQUE (surname_name))")

   dbSendQuery(base_diet, "CREATE TABLE Votings (id_voting int NOT NULL PRIMARY KEY,
               nr_meeting int NOT NULL, date_meeting date NOT NULL,
               nr_voting int NOT NULL, topic_voting ntext NOT NULL,
               link_results nvarchar(200))")

   dbSendQuery(base_diet, "CREATE TABLE Votes (id_vote int NOT NULL PRIMARY KEY,
      id_deputy varchar(4) NOT NULL, id_voting int NOT NULL, vote nvarchar(20) NOT NULL,
      club nvarchar(50), FOREIGN KEY (id_deputy) REFERENCES Deputies(id_deputy),
      FOREIGN KEY (id_voting) REFERENCES Votings(id_voting))")

   suppressWarnings(dbDisconnect(base_diet))
   return(invisible(NULL))
}

#tests
base_path<-"H:\\R\\staz sejm\\base_diet.sql"
base_diet <- dbConnect(SQLite(), dbname = base_path)
dbListTables(base_diet)
dbReadTable(base_diet,"Deputies")
dbReadTable(base_diet,"Votings")
dbReadTable(base_diet,"Votes")
dbSendQuery(base_diet,"INSERT INTO Deputies VALUES (1,'Jan Kowalski')")
dbSendQuery(base_diet,"INSERT INTO Deputies VALUES (2,'Jan Kowalski')") #error, ze nieunikalny
dbReadTable(base_diet,"Deputies")
suppressWarnings(dbDisconnect(base_diet))

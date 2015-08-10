library("rvest")
library("stringi")

database_path<-"H:\\R\\staz sejm\\database_diet.sqlite"

srp_deputies_add_new <- function(dbname,user,password,host,type,id){

  #getting data from page with deputies
  deputies <- srp_deputies_data(type)

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
}

srp_deputies_list_update <- function(dbname,user,password,host){

  #checking last id of deputies
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  last_id <- fetch(dbSendQuery(database_diet, "SELECT max(id_deputy) FROM deputies"))
  last_id <- as.character(last_id)
  suppressWarnings(dbDisconnect(database_diet))

  #adding new deputies to database
  srp_deputies_add_new(dbname,user,password,host,"active",last_id)
  srp_deputies_add_new(dbname,user,password,host,"inactive",last_id)

  return(invisible(NULL))
}


#tests
drv <- dbDriver("PostgreSQL")
database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
dbReadTable(database_diet,"deputies")
dbSendQuery(database_diet,"DELETE FROM deputies WHERE id_deputy>'300'")
dbReadTable(database_diet,"deputies")
suppressWarnings(dbDisconnect(database_diet))
srp_deputies_list_update(dbname,user,password,host)
database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
dbReadTable(database_diet,"deputies")
suppressWarnings(dbDisconnect(database_diet))


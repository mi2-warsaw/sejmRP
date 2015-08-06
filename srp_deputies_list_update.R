library("rvest")
library("stringi")

database_path<-"H:\\R\\staz sejm\\database_diet.sqlite"

srp_deputies_add_new <- function(database_path,type,id){
  
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
    database_diet <- dbConnect(SQLite(), dbname = database_path)
    n <- length(new_deputies)
    for(i in seq_len(n)){
      dbSendQuery(database_diet,paste0("INSERT INTO Deputies (id_deputy, surname_name) VALUES (",
                                       "\"",id_new_deputies[i],"\",\"",new_deputies[i],"\")"))
    }
    suppressWarnings(dbDisconnect(database_diet))
  }
}

srp_deputies_list_update <- function(database_path){
  
  #checking last id of deputies
  database_diet <- dbConnect(SQLite(), dbname = database_path)
  last_id <- fetch(dbSendQuery(database_diet, "SELECT max(id_deputy) FROM Deputies"))
  last_id <- as.character(last_id)
  suppressWarnings(dbDisconnect(database_diet))
  
  #adding new deputies to database
  srp_deputies_add_new(database_path,"active",last_id)
  srp_deputies_add_new(database_path,"inactive",last_id)
  
  return(invisible(NULL))
}


#tests
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbReadTable(database_diet,"Deputies")
dbSendQuery(database_diet,"DELETE FROM Deputies WHERE id_deputy>450")
dbReadTable(database_diet,"Deputies")
suppressWarnings(dbDisconnect(database_diet))
srp_deputies_list_update(database_path)
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbReadTable(database_diet,"Deputies")
suppressWarnings(dbDisconnect(database_diet))


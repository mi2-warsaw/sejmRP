library("rvest")
library("stringi")

database_path<-"H:\\R\\staz sejm\\database_diet.sql"

srp_deputies_list_update <- function(database_path){
  
  #checking last id of deputies
  database_diet <- dbConnect(SQLite(), dbname = database_path)
  last_id <- fetch(dbSendQuery(database_diet, "SELECT max(id_deputy) FROM Deputies"))
  last_id <- as.integer(last_id)
  
  #getting data from page with active deputies
  page_active <- html("http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=A")
  deputies_active_data <- html_nodes(page_active, "#contentBody li")
  
  #getting ids of active deputies
  links_active <- html_nodes(deputies_active_data, "a")
  links_active <- html_attrs(links_active)
  links_active <- sapply(links_active,function(element){
    element[2]
  })
  id_deputies_active <- unlist(stri_extract_all_regex(links_active,"(?<=id=)[0-9]+"))
  
  #finding maximum id of active deputies
  max_id <- max(id_deputies_active)
  
  #if there is more deputies than in database we add them
  if(max_id>last_id){
    #getting names and surnnames of active deputies
    deputies_active <- html_nodes(deputies_active_data, ".deputyName")
    deputies_active <- html_text(deputies_active)
    
    #finding deputies with higher ids
    which_new_deputies <- which(id_deputies_active>last_id)
    id_new_deputies <- id_deputies_active[which_new_deputies]
    new_deputies <- deputies_active[which_new_deputies]
    
    #adding new deputies to database
    n <- length(new_deputies)
    for(i in seq_len(n)){
      dbSendQuery(database_diet,paste0("INSERT INTO Deputies (id_deputy, surname_name) VALUES (",
                                       "\"",id_new_deputies[i],"\",\"",new_deputies[i],"\")"))
    }
  }
  
  #disconnecting to database
  suppressWarnings(dbDisconnect(database_diet))
  
  return(invisible(NULL))
}


#tests
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbReadTable(database_diet,"Deputies")
dbSendQuery(database_diet,"DELETE FROM Deputies WHERE id_deputy>500")
dbReadTable(database_diet,"Deputies")
srp_deputies_list_update(database_path)
dbReadTable(database_diet,"Deputies")
suppressWarnings(dbDisconnect(database_diet))

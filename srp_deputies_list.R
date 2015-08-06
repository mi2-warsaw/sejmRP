library("rvest")
library("stringi")

database_path<-"H:\\R\\staz sejm\\database_diet.sql"

srp_deputies_list <- function(database_path){
  
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
  
  #getting names and surnnames of active deputies
  deputies_active <- html_nodes(deputies_active_data, ".deputyName")
  deputies_active <- html_text(deputies_active)
  
  #getting data from page with inactive deputies
  page_inactive <- html("http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=B")
  deputies_inactive_data <- html_nodes(page_inactive, "#contentBody li")
  
  #getting ids of inactive deputies
  links_inactive <- html_nodes(deputies_inactive_data, "a")
  links_inactive <- html_attrs(links_inactive)
  links_inactive <- sapply(links_inactive,function(element){
    element[2]
  })
  id_deputies_inactive <- unlist(stri_extract_all_regex(links_inactive,"(?<=id=)[0-9]+"))
  
  #getting names and surnnames of inactive deputies
  deputies_inactive <- html_nodes(deputies_inactive_data, ".deputyName")
  deputies_inactive <- html_text(deputies_inactive)
  
  #merging ids, names and surnames of active and inactive deputies
  id_deputies <- c(id_deputies_active,id_deputies_inactive)
  deputies <- c(deputies_active,deputies_inactive)
  
  #putting this data frame to database
  database_diet <- dbConnect(SQLite(), dbname = database_path)
  n <- length(deputies)
  for(i in seq_len(n)){
    dbSendQuery(database_diet,paste0("INSERT INTO Deputies (id_deputy, surname_name) VALUES (",
                                     "\"",id_deputies[i],"\",\"",deputies[i],"\")"))
  }
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}

#tests
srp_deputies_list(database_path)
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbReadTable(database_diet,"Deputies")
suppressWarnings(dbDisconnect(database_diet))

#tests with encoding
database_diet <- dbConnect(SQLite(), dbname = database_path)
dbListTables(database_diet)
dbReadTable(database_diet,"Deputies")

stri_enc_isutf8(dbReadTable(database_diet,"Deputies")[,2])
Encoding(deputies)
Encoding(dbReadTable(database_diet,"Deputies")[,2])

library("rvest")
library("stringi")

base_path<-"H:\\R\\staz sejm\\base_diet.sql"

function srp_lista_poslow(base_path){
  
}

page_active <- html("http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=A") 
deputies_active_data <- html_nodes(page_active, "#contentBody li")

links_active <- html_nodes(deputies_active_data, "a")
links_active <- html_attrs(links_active)
links_active <- sapply(links_active,function(element){
  element[2]
})

id_deputies_active <- unlist(stri_extract_all_regex(links_active,"(?<=id=)[0-9]+"))

deputies_active <- html_nodes(deputies_active_data, ".deputyName")
deputies_active <- html_text(deputies_active)

page_inactive <- html("http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=B") 
deputies_inactive_data <- html_nodes(page_inactive, "#contentBody li")

links_inactive <- html_nodes(deputies_inactive_data, "a")
links_inactive <- html_attrs(links_inactive)
links_inactive <- sapply(links_inactive,function(element){
  element[2]
})

id_deputies_inactive <- unlist(stri_extract_all_regex(links_inactive,"(?<=id=)[0-9]+"))

deputies_inactive <- html_nodes(deputies_inactive_data, ".deputyName")
deputies_inactive <- html_text(deputies_inactive)

id_deputies <- c(id_deputies_active,id_deputies_inactive)
deputies <- c(deputies_active,deputies_inactive)

deputies_df <- data.frame(id_deputy=id_deputies, surname_name=deputies)


base_diet <- dbConnect(SQLite(), dbname = base_path)
dbWriteTable(base_diet,"Deputies",deputies_df,overwrite=TRUE)

# dbSendQuery(base_diet,"INSERT INTO Deputies (id_deputy, surname_name) SELECT id_deputy, surname_name FROM Temporary")
# dbRemoveTable(baza_seriali,"Temporary")

dbListTables(base_diet)
dbReadTable(base_diet,"Deputies")
# dbReadTable(base_diet,"Temporary")

stri_enc_isutf8(deputies_df[,2])
stri_enc_isutf8(dbReadTable(base_diet,"Deputies")[,2])
deputies_df[,2]<-stri_enc_toutf8(deputies_df[,2])

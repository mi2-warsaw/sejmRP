library("rvest")
library("stringi")

database_path<-"H:\\R\\staz sejm\\database_diet.sqlite"

srp_deputies_data <- function(type){

  #choosing proper page of deputies
  if(type=="active"){
    page <- "http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=A"
  } else if(type=="inactive"){
    page <- "http://www.sejm.gov.pl/Sejm7.nsf/poslowie.xsp?type=B"
  }

  #getting data from page with deputies
  page <- html(page)
  deputies_data <- html_nodes(page, "#contentBody li")

  #getting ids of deputies
  links <- html_nodes(deputies_data, "a")
  links <- html_attrs(links)
  links <- sapply(links,function(element){
    element[2]
  })
  id_deputies <- unlist(stri_extract_all_regex(links,"(?<=id=)[0-9]+"))

  #getting names and surnnames of deputies
  deputies <- html_nodes(deputies_data, ".deputyName")
  deputies <- html_text(deputies)

  #creating data frame with deputies data
  deputies_df <- data.frame(id_deputy=id_deputies,surname_name=deputies,stringsAsFactors=FALSE)

  return(deputies_df)
}

srp_deputies_list_create <- function(dbname,user,password,host){

  #getting data from page with active deputies
  deputies_active <- srp_deputies_data("active")

  #getting data from page with inactive deputies
  deputies_inactive <- srp_deputies_data("inactive")

  #merging ids, names and surnames of active and inactive deputies
  id_deputies <- c(deputies_active[,1],deputies_inactive[,1])
  deputies <- c(deputies_active[,2],deputies_inactive[,2])

  #putting this data frame to database
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  n <- length(deputies)
  for(i in seq_len(n)){
    dbSendQuery(database_diet,paste0("INSERT INTO deputies (id_deputy, surname_name) VALUES (",
                                     "'",id_deputies[i],"','",deputies[i],"')"))
  }
  suppressWarnings(dbDisconnect(database_diet))
  return(invisible(NULL))
}

#tests
dbname<-"sejmrp"
user<-"sejmrp"
password<-"pQwZ99Kj65x"
host<-"services.mini.pw.edu.pl"


srp_deputies_data("active")
srp_deputies_data("inactive")

srp_deputies_list_create(database_path)
drv <- dbDriver("PostgreSQL")
database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
dbReadTable(database_diet,"deputies")
suppressWarnings(dbDisconnect(database_diet))

#tests with encoding
database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
dbListTables(database_diet)
dbReadTable(database_diet,"deputies")

stri_enc_isutf8(dbReadTable(database_diet,"deputies")[,2])
Encoding(deputies)
Encoding(deputies[1])<-"UTF-8"
Encoding(dbReadTable(database_diet,"deputies")[,2])

fetch(dbReadTable(database_diet,"Deputies")[,2])
stri_enc_detect(dbReadTable(database_diet,"deputies")[1,2])
stri_enc_isutf8(dbReadTable(database_diet,"deputies")[1,2])
stri_enc_toutf8(dbReadTable(database_diet,"deputies")[512,2])
stri_encode(deputies[1],to="UTF-32")
stri_enc_set("UTF-8")

dbGetQuery(database_diet,"SHOW client_encoding")
dbReadTable(database_diet,"deputies")[512,2]==deputies[512]

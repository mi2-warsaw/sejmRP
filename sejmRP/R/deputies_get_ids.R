#opis

deputies_get_ids <- function(dbname,user,password,host,windows=TRUE){
  #stopifnoty
  
  #jesli windows to zmieniamy kodowanie
  
  #getting deputies' ids
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  deputies_table <- dbReadTable(database_diet,"deputies")
  deputies_ids <- deputies_table[,1]
  
  #calling deputies' ids with their names and surnames
  if(windows){
    names(deputies_ids) <- iconv(deputies_table[,2],from="UTF-8",to="Windows-1250")
  }
  else {
    names(deputies_ids) <- deputies_table[,2]
  }
  suppressWarnings(dbDisconnect(database_diet))
  
  return(deputies_ids)
}


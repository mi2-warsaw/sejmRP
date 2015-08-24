#opis

votings_create_table <- function(dbname,user,password,host,home_page,page){
  #stopifnoty
  
  #getting meetings table with meetings' numbers
  meetings_table <- votings_get_meetings_table(page)
  
  #getting meetings links with votings
  meetings_links <- votings_get_meetings_links(home_page,page)
  
  #remembering first voting id
  id_voting <- 1
  
  for(i in rev(seq_len(length(meetings_links)))){
    #getting meetings date
    meetings_date <- votings_get_date(meetings_links[i])
    
    #getting votings table with votings' numbers and topics
    votings_table <- votings_get_votings_table(meetings_links[i])
    
    #getting votings links
    votings_links <- votings_get_votings_links(home_page,meetings_links[i])
    
    #putting this data frame to database
    drv <- dbDriver("PostgreSQL")
    database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
    for(j in rev(seq_len(length(votings_links)))){
      dbSendQuery(database_diet,paste0("INSERT INTO votings (id_voting, nr_meeting, date_meeting,",
               "nr_voting, topic_voting, link_results) VALUES (", id_voting , ",", meetings_table[i,1], 
               ",'", meetings_date, "',", votings_table[j,1], ",'", votings_table[j,3], "','", 
               votings_links[j], "')"))
      
      id_voting <- id_voting + 1
    }
    suppressWarnings(dbDisconnect(database_diet))
  }
  
  return(invisible(NULL))
}
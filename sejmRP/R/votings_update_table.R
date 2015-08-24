#opis

votings_update_table <- function(dbname,user,password,host,home_page,page){
  #stopifnoty
  
  #checking last nr_meeting, removing records with that number and checking last id_voting
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  last_nr_meeting <- fetch(dbSendQuery(database_diet, "SELECT max(nr_meeting) FROM votings"))
  last_nr_meeting <- as.integer(last_nr_meeting)
  dbSendQuery(database_diet,paste0("DELETE FROM votings WHERE nr_meeting=",last_nr_meeting))
  last_id_voting <- fetch(dbSendQuery(database_diet, "SELECT max(id_voting) FROM votings"))
  last_id_voting <- as.integer(last_id_voting)
  suppressWarnings(dbDisconnect(database_diet))
  
  #getting meetings table with meetings' numbers
  meetings_table <- votings_get_meetings_table(page)
  
  #getting meetings links with votings
  meetings_links <- votings_get_meetings_links(home_page,page)
  
  #choosing the place from where we start update table
  which_to_update <- which(meetings_table[,1]>=last_nr_meeting)
  meetings_table <- meetings_table[which_to_update,]
  meetings_links <- meetings_links[which_to_update]
  
  #remembering first new voting id
  id_voting <- last_id_voting + 1
  
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
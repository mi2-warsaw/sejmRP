#opis

votes_update_table <- function(dbname,user,password,host,home_page,windows=TRUE){
  #stopifnoty
  
  #checking last nr_meeting, removing records with that number and checking last id_voting
  drv <- dbDriver("PostgreSQL")
  database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
  last_id_voting <- fetch(dbSendQuery(database_diet, "SELECT max(id_voting) FROM votes"))
  last_id_voting <- as.integer(last_id_voting)
  dbSendQuery(database_diet,paste0("DELETE FROM votes WHERE id_voting=",last_id_voting))
  last_id_vote <- fetch(dbSendQuery(database_diet, "SELECT max(id_vote) FROM votes"))
  last_id_vote <- as.integer(last_id_vote)
  
  #getting voting_id and results links
  votings_ids_links <- dbReadTable(database_diet,"votings")[,c(1,6)]
  suppressWarnings(dbDisconnect(database_diet))
  
  #choosing the place from where we start update table
  which_to_update <- which(votings_ids_links[,1]>=last_id_voting)
  votings_ids_links <- votings_ids_links[which_to_update,]
  
  #remembering first new vote id
  id_vote <- last_id_vote + 1
  
  for(i in seq_len(nrow(votings_ids_links))){
    #getting clubs and links from voting
    votes_get_clubs <- votes_get_clubs_links(home_page,votings_ids_links[i,2])
    
    for(j in seq_len(nrow(votes_get_clubs))){
      #getting deputies id, vote and club
      votes_info <- votes_match_deputies_ids(dbname,user,password,host,votes_get_clubs[j,2],windows)
      
      #putting this data frame to database
      database_diet <- dbConnect(drv,dbname=dbname,user=user,password=password,host=host)
      for(k in seq_len(nrow(votes_info))){
        dbSendQuery(database_diet,paste0("INSERT INTO votes (id_vote, id_deputy, id_voting, vote,",
                                         "club) VALUES (", id_vote , ",'", votes_info[k,3], "',", votings_ids_links[i,1], ",'",
                                         votes_info[k,2], "','", votes_get_clubs[j,1], "')"))
        
        id_vote <- id_vote + 1
      }
      suppressWarnings(dbDisconnect(database_diet))
    }
  }
  
  return(invisible(NULL))
}
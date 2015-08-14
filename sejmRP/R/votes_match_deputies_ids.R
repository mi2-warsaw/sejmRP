#opis

votes_match_deputies_ids <- function(dbname,user,password,host,page,windows=TRUE){
  #stopifnoty
  
  #getting all of deputies' ids
  deputies_whole_ids <- deputies_get_ids(dbname,user,password,host,windows)
  
  #getting votes' results
  votes_results <- votes_get_results(page)
  
  #getting deputies' (from voting) ids 
  deputies_ids <- unname(deputies_whole_ids[votes_results[,1]])
  
  #creating data frame with data
  votes_deputies_ids <- cbind(votes_results,id=deputies_ids)
  
  return(votes_deputies_ids)
}
#opis

votes_get_results <- function(page){
  #stopifnoty
  
  #getting deputies and their votes
  votes_clubs_results <- readHTMLTable(page, encoding = "UTF-8", stringsAsFactors = FALSE)[[1]]
  deputies <- c(votes_clubs_results[,2],votes_clubs_results[,5])
  deputies <- deputies[!is.na(deputies)]
  deputies_votes <- c(votes_clubs_results[,3],votes_clubs_results[,6])
  deputies_votes <- deputies_votes[!is.na(deputies_votes)]
  
  #creating data frame with data
  votes_results <- data.frame(deputy=deputies, vote=deputies_votes, stringsAsFactors = FALSE)
  
  return(votes_results)
}
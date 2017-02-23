#' Gets the Most Frequent Club for Each Deputy
#' 
#' One deputy may belong to many different clubs and change clubs over time. 
#' The function \code{get_most_frequent_club} calculates the most frequent club for each deputy.
#'
#' @param deputy_id a vector with deputy unique ids (may be also a vactor with characters name/surname)
#' @param club vector of length equal to \code{deputy_id} with club memberships for particular deputies
#'
#' @return a data frame with club memberships of deputies
#'
#' @examples
#' # votes <- get_filtered_votes(terms_of_office = c(7,7))
#' data(votes)
#' clubs <- get_most_frequent_club(votes$surname_name, votes$club)
#' head(clubs)
#' 
#' @author Przemyslaw Biecek
#' @import dplyr
#' @export

get_most_frequent_club <- function(deputy_id, club) {
  df <- data.frame(surname_name = deputy_id, club = club)
  df %>%
       group_by(surname_name, club) %>%
       summarise(n = n()) %>% 
       arrange(-n) %>%
       group_by(surname_name) %>%
       top_n(1) %>%
       as.data.frame() -> clubs
  row.names(clubs) <- clubs[,1]

  return(clubs)
}

#' Converts Voting Table into the Deputies Distance Matrix
#'
#' Function \code{get_distance_matrix} converts a data frame with tree columns (deputies' ids, votings' ids and votes) 
#' into a deputies distance matrix.
#'
#' @details
#' Function \code{get_distance_matrix} calculated distances among deputies based on their 
#' votes The more similar are the voting the smaller distance between deputies.
#' 
#' @param votes a data frame with three columns, respectively: deputy id, voting id, vote
#' @param weights if supplied it should be a named vector that converts votes into a numeric values that correspond to their similarity
#' @param allowMissings maximum number of missing votigs allowd for deputy.
#'
#' @return distance matrix
#'
#' @examples
#' \dontrun{
#' # votes <- get_filtered_votes(terms_of_office = c(7,7))
#' data(votes)
#' mat1 <- get_distance_matrix(votes[,c("surname_name", "id_voting", "vote")])
#' v <- c(`Za` = 5, `Przeciw` = -5, `Wstrzymał się` = 2, `Nieobecny` = 0)/10
#' mat2 <- get_distance_matrix(votes[,c("surname_name", "id_voting", "vote")], weights = v)
#' }
#' 
#' @author Przemyslaw Biecek
#' @import dplyr
#' @import tidyr
#' @import cluster
#' @export
get_distance_matrix <- function(votes, weights = NULL, allowMissings = 0) {
  stopifnot(is.data.frame(votes), ncol(votes) == 3)
  
  colnames(votes) = c("id_deputy", "id_voting", "vote")
  votesSpread <- spread(votes, id_voting, vote)
  
  # remove missings
  keep_idx <- which(rowMeans(is.na(votesSpread)) <= allowMissings)
  votesSpread <- votesSpread[keep_idx,]
  tmp <- as.matrix(votesSpread[,-1])

  if (is.null(weights)) { 
    # votes are threated as nominal values
    mat <- matrix(0, nrow = nrow(tmp), ncol = nrow(tmp))
    if (nrow(tmp) > 1) {
      for (i in 1:(nrow(mat)-1)) {
        for (j in (i+1):ncol(mat)) {
          mat[i,j] <- sum(tmp[i,] != tmp[j,], na.rm = TRUE)/max(1, sum(!is.na(tmp[i,]) & !is.na(tmp[j,])))
          mat[j,i] <- mat[i,j]
        }
      }
      # rescaling
      mat <- mat * ncol(tmp)
    }
  } else { 
    # convert to numbers 
    tmp2 <- apply(tmp, 2, function(x) v[x])
    mat <- as.matrix(dist(tmp2))
  }
  colnames(mat) <- votesSpread[,1]
  rownames(mat) <- votesSpread[,1]

  return(as.dist(mat))
}

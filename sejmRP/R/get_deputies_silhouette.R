#' Converts Deputies Distance Matrix into a Silhouette Plot
#'
#' Function \code{get_deputies_silhouette} converts the distance matrix between deputies 
#' and their clubs into a silhouette plot
#'
#' @param distances a distance matrix, preferably created with \code{get_distance_matrix}
#' @param plot if \code{TRUE} then the \code{ggplot} object will be returned instead of the dendrogram itself
#' @param clubs a data.frame that maps \code{deputy_id} (names from \code{distances}) on clubs. 
#' It should contain at least one column - \code{club} and rows should have names corresponding to names in the \code{distances}. 
#' @param remove_missing_clubs if TRUE then rows of \code{distances} that are not mapped in \code{clubs} will be removed
#'
#' @return silhouette object or a ggplot 
#'
#' @examples
#' \dontrun{
#' # votes <- get_filtered_votes(terms_of_office = c(7,7))
#' data(votes)
#' v <- c(`Za` = 5, `Przeciw` = -5, `Wstrzymał się` = 2, `Nieobecny` = 0)/10
#' mat2 <- get_distance_matrix(votes[,c("surname_name", "id_voting", "vote")], weights = v)
#' df <- votes[,c("surname_name","club")]
#' df %>%
#'   group_by(surname_name, club) %>%
#'   summarise(n = n()) %>% 
#'   arrange(-n) %>%
#'   group_by(surname_name) %>%
#'   top_n(1) %>%
#'   as.data.frame() -> clubs
#' row.names(clubs) <- clubs[,1]
#' 
#' get_deputies_silhouette(mat2, clubs)
#' }
#' 
#' @author Przemyslaw Biecek
#' @export
get_deputies_silhouette <- function(distances, clubs = NULL, plot = TRUE) {
  stopifnot(any(c("dist","matrix") %in% class(distances)),
            is.logical(plot))
  
  # remove zeros
  distances <- as.matrix(distances)
  rnam <- rownames(distances)
  cluster <- clubs[rnam,"club"]
  if (remove_missing_clubs) {
    idx <- which(!is.na(cluster))
    distances <- distances[idx, idx]
    cluster <- cluster[idx]
  }
  
  sil <- silhouette(cluster, distances)
  # return last value
  if (plot) {
    fviz_silhouette(sil)
  } else {
    sil  
  }
}

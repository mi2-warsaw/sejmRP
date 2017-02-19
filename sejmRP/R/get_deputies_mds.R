#' Converts Deputies Distance Matrix into a Multidimensional Scaling Plot
#'
#' Function \code{get_deputies_mds} converts the distance matrix between deputies 
#' into a 2D representation (Multidimensional Scaling)
#'
#' @param distances a distance matrix, preferably created with \code{get_distance_matrix}
#' @param plot if \code{TRUE} then the \code{ggplot} object will be returned instead of the dendrogram itself
#' @param clubs a matrix that maps \code{deputy_id} (names from \code{distances}) on clubs
#'
#' @return MDS coordinates or a ggplot 
#'
#' @examples
#' \dontrun{
#' votes <- get_deputies_mds(terms_of_office = c(7,7))
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
#' get_deputies_mds(mat2, clubs)
#' }
#' 
#' @author Przemyslaw Biecek
#' @import MASS
#' @export
get_deputies_mds <- function(distances, clubs = NULL, plot = TRUE) {
  stopifnot(any(c("dist","matrix") %in% class(distances)),
            is.logical(plot))
  
  # remove zeros
  distances <- as.matrix(distances)
  rnam <- rownames(distances)
  distances <- distances + min(c(distances[distances > 0], 1))
  diag(distances) <- 0
  nv <- isoMDS(distances, k=2)
  object <- list(data = as.data.frame(nv$points), cluster = clubs[rnam,"club"])
  
  fviz_cluster(object, geom="point")
}

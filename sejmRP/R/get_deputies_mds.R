#' Converts Deputies Distance Matrix into a Multidimensional Scaling Plot
#'
#' Function \code{get_deputies_mds} converts the distance matrix between deputies 
#' into a 2D representation (Multidimensional Scaling)
#'
#' @param distances a distance matrix, preferably created with \code{get_distance_matrix}
#' @param plot if \code{TRUE} then the \code{ggplot} object will be returned instead of the dendrogram itself
#' @param clubs a data.frame that maps \code{deputy_id} (names from \code{distances}) on clubs. 
#' It should contain at least one column - \code{club} and rows should have names corresponding to names in the \code{distances}. 
#' @param remove_missing_clubs if TRUE then rows of \code{distances} that are not mapped in \code{clubs} will be removed
#'
#' @return MDS coordinates or a ggplot 
#'
#' @examples
#' # votes <- get_filtered_votes(terms_of_office = c(7,7))
#' library(dplyr)
#' library(ggplot2)
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
#' clubs$club[clubs$club == "niez."] = "cross-bencher"
#' get_deputies_mds(mat2, clubs)
#' 
#' # without cross bencher deputies
#' clubs2 <- clubs[clubs$club != "cross-bencher",]
#' get_deputies_mds(mat2, clubs2)
#' 
#' @author Przemyslaw Biecek
#' @importFrom MASS isoMDS
#' @export
get_deputies_mds <- function(distances, clubs = NULL, plot = TRUE, remove_missing_clubs = TRUE) {
  stopifnot(any(c("dist","matrix") %in% class(distances)),
            is.logical(plot))
  
  # remove zeros
  distances <- as.matrix(distances)
  rnam <- rownames(distances)
  distances <- distances + min(c(distances[distances > 0], 1))
  diag(distances) <- 0
  cluster <- clubs[rnam,"club"]
  if (remove_missing_clubs) {
    idx <- which(!is.na(cluster))
    distances <- distances[idx, idx]
    cluster <- cluster[idx]
  }
  cluster <- droplevels(cluster)
  
  nv <- isoMDS(distances, k=2)

  # return last value
  if (plot) {
    object <- list(data = as.data.frame(nv$points), cluster = cluster)
    fviz_cluster(object, geom="point") + 
       coord_fixed() + xlab("") + ylab("") + theme_minimal() + 
       ggtitle("MDS plot for deputies voting profiles")
  } else {
    nv
  }
}

#' Converts Deputies Distance Matrix into a Deputies Dendrogram
#'
#' Function \code{get_deputies_dendrogram} converts the distance matrix between deputies 
#' into a dendrogram or ggplot of the dendrogram.
#'
#' @param distances a distance matrix, preferably created with \code{get_distance_matrix}
#' @param plot if \code{TRUE} then the \code{ggplot} object will be returned instead of the dendrogram itself
#' @param method clustering method, see the \code{agnes} for more details
#' @param k number of groups, will be passed to \code{fviz_dend}
#'
#' @return dendrogram or a ggplot 
#'
#' @examples
#' \dontrun{
#' # votes <- get_filtered_votes(terms_of_office = c(7,7))
#' data(votes)
#' v <- c(`Za` = 5, `Przeciw` = -5, `Wstrzymał się` = 2, `Nieobecny` = 0)/10
#' mat2 <- get_distance_matrix(votes[,c("surname_name", "id_voting", "vote")], weights = v)
#' get_deputies_dendrogram(mat2, k=5)
#' }
#' 
#' @author Przemyslaw Biecek
#' @import factoextra
#' @export
get_deputies_dendrogram <- function(distances, plot = TRUE, method = "ward", k = NULL) {
  stopifnot(any(c("dist","matrix") %in% class(distances)),
            is.logical(plot))
  
  clust <- agnes(distances, method = method)
  if (!plot) return(clust)

  factoextra::fviz_dend(clust, k = k, main = "")
}

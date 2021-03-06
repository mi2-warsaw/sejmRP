% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_deputies_dendrogram.R
\name{get_deputies_dendrogram}
\alias{get_deputies_dendrogram}
\title{Converts Deputies Distance Matrix into a Deputies Dendrogram}
\usage{
get_deputies_dendrogram(distances, plot = TRUE, method = "ward", k = NULL)
}
\arguments{
\item{distances}{a distance matrix, preferably created with \code{get_distance_matrix}}

\item{plot}{if \code{TRUE} then the \code{ggplot} object will be returned instead of the dendrogram itself}

\item{method}{clustering method, see the \code{agnes} for more details}

\item{k}{number of groups, will be passed to \code{fviz_dend}}
}
\value{
dendrogram or a ggplot
}
\description{
Function \code{get_deputies_dendrogram} converts the distance matrix between deputies 
into a dendrogram or ggplot of the dendrogram.
}
\examples{
# votes <- get_filtered_votes(terms_of_office = c(7,7))
data(votes)
v <- c(`Za` = 5, `Przeciw` = -5, `Wstrzymał się` = 2, `Nieobecny` = 0)/10
mat2 <- get_distance_matrix(votes[,c("surname_name", "id_voting", "vote")], weights = v)
get_deputies_dendrogram(mat2, k=5)

}
\author{
Przemyslaw Biecek
}

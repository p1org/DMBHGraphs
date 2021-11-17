#' Finds reciprocated edges
#' 
#' @details
#' Finds reciprocated edges by checking if the sending vertex u
#' is in the out-neighborhood of its out-neighbors, 
#' i.e. is \eqn{u} in \eqn{N(v)} for \eqn{v} in \eqn{N(u)}
#' 
#' @param g igraph directed graph
#' 
#' @return vector of booleans
reciprocated_edges <- function(g) {
  apply(
    X=igraph::ends(g, igraph::E(g), names=FALSE),
    MARGIN=1,
    FUN = function(e,g){e[1] %in% igraph::neighbors(g, e[2], mode="out")},
    g=g
  )
}

#' Split graph into reciprocated and unreciprocated parts
#' 
#' @param g igraph directed graph
#' 
#' @return List containing the following: 
#' \itemize{
#'  \item \code{gudir}: graph containing the reciprocated edges is 
#'    represented by an igraph undirected graph object. 
#'  \item \code{gdir}: graph containing the unreciprocated edges 
#'    is represented by an igraph directed graph. 
#' }
split_directed <- function(g) {
  recip_idx <- reciprocated_edges(g)
  return(
    list(
      gudir = igraph::as.undirected(
        igraph::subgraph.edges(
          g, 
          igraph::E(g)[recip_idx], 
          delete.vertices = FALSE
        ), mode = "collapse"
      ),
      gdir = igraph::subgraph.edges(
        g, 
        igraph::E(g)[!recip_idx], 
        delete.vertices = FALSE
      )
    )
  )
}

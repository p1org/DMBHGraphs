#' Reverses the direction a directed path
#' 
#' @description
#' Takes a list of edges and connects the head of each to the
#' tail of the previous edge.
#' 
#' @details
#' This set of edges is obtained by joining the head of \eqn{e[i+1]}
#' with the tail of \eqn{e[i]} for \eqn{i} through \eqn{m-1} and joining 
#' the tail of \eqn{e[1]} with the head of \eqn{e[m]}. 
#' 
#' The edges in \code{edges} must be from \code{g}. 
#' 
#' @seealso \code{\link{bipartite_walk}}
#' 
#' @param g igraph graph object from which \code{edges} came from
#' @param edges graph.es object, edge sequence from g
#' 
#' @return list
reverse_walk <- function(g, edges) {

  edge_ends <- igraph::ends(g, edges)
  m <- length(edges)
  new_edges <- matrix(nrow = m, ncol = 2)

  new_edges[m, 1] <- edge_ends[1, 1]
  new_edges[m, 2] <- edge_ends[m, 2]

  for (i in 1:(m - 1)) {
    new_edges[i, 1] <- edge_ends[i + 1, 1]
    new_edges[i, 2] <- edge_ends[i, 2]
  }

  return(new_edges)
}

#' Wrapper for reverse_walk
#' 
#' Applies \code{reverse_walk} and converts output to igraph graph and checks for simplicity.
#' 
#' @details
#' Takes a list of edges and calls \code{reverse_walk}. 
#' 
#' This function returns \code{NULL} if the subgraph induced by
#' the resulting edges contains loops or multi-edges.
#'  
#' @seealso \code{\link{reverse_walk}}
#' 
#' @param g igraph graph
#' @param edges graph.es object representing an edge sequence from g
#' 
#' @return list or \code{NULL}
bipartite_walk <- function(g, edges) {

  edges_to_add <- reverse_walk(g, edges)
  subgraph_to_add <- igraph::graph_from_edgelist(edges_to_add)

  if (!igraph::is.simple(subgraph_to_add)) {
    return(NULL)
  }

  return(edges_to_add)
}


check_structural_zeros <- function(b, zeros.graph) {
  
    if (!igraph::is.directed(zeros.graph)) {
      b <- igraph::as.undirected(b, mode = "collapse")
    }

    n_common_edges <- igraph::ecount(
      igraph::intersection(b, zeros.graph)
    )

    if (n_common_edges > 0) {
      return(FALSE)
    }

    return(TRUE)
}

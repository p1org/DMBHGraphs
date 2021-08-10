#' reverses the direction of an edge list
#' 
#' Takes a list of edges and connects connects the head of each to the
#' head of the previous edge.
#' 
#' This set of edges is obtained by joining the head of e[i+1] with the tail 
#' of e[i] for i through m-1 and joining the tail of e[1] with the head of e[m]
#' 
#' @param g igraph graph object from where the edges came from
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

# TODO: remove zeros.graph parameter
#' wrapper for reverse_walk
#' 
#' Takes a list of edges and calls \code{reverse_walk}. If the subgraph induced by
#' the resulting edges contains loops or multi-edges, this function returns NULL.
#' If the resulting subgraph contains a structural zero, this function returns NULL.
#' 
#' @section Details
#' 
#' The edgelist passed to this function is always directed. However the graph representing
#' the structural zeros graph (\code{zeros.graph} argument) can be directed or undirected. 
#' When this graph is undirected, the edges in the graph represent bidirected edges in the 
#' main graph. The logic for determining which case is applicable is determined by the calling
#' function. 
#' 
#' 
#' @param g igraph graph
#' @param edges graph.es object, edge sequence from g
#' @param zeros.graph igraph graph or NULL
#' 
#' @return list or NULL
bipartite_walk <- function(g, edges, zeros.graph = NULL) {

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

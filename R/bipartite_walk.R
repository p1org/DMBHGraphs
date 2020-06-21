#' reverses the direction of an edge list
#' 
#' Takes a list of edges and connects connects the head of each to the
#' head of the previous edge.
#' 
#' This set of edges is obtained by joining the head of e[i+1] with the tail 
#' of e[i] for i through m-1 and joining the tail of e[1] with the head of e[m]
#' 
#' @param edges list
#' 
#' @return list
reverse_walk <- function(edges) {

  m <- nrow(edges)
  new_edges <- matrix(nrow = m, ncol = 2)

  new_edges[m, 1] <- edges[1, 1]
  new_edges[m, 2] <- edges[m, 2]

  for (i in 1:(m - 1)) {
    new_edges[i, 1] <- edges[i + 1, 1]
    new_edges[i, 2] <- edges[i, 2]
  }

  return(new_edges)
}

#' wrapper for reverse_walk
#' 
#' Takes a list of edges and calls `reverse_walk`. If the subgraph induced by
#' the resulting edges contains loops or multi-edges, this function returns NULL.
#' If the resulting contain a structural zero, this function returns NULL.
#' 
#' @param edges list
#' @param zeros.graph igraph graph or NULL
#' 
#' @return list or NULL
bipartite_walk <- function(edges, zeros.graph=NULL) {

  edges_to_add <- reverse_walk(edges)
  subgraph_to_add <- igraph::graph_from_edgelist(edges_to_add)

  if (!igraph::is.simple(subgraph_to_add)) {
    return(NULL)
  }

  # if move to using igragh edge sequences, see "graph.es" object and "intersection" method
  if (!is.null(zeros.graph)) {
    n_common_edges <- igraph::ecount(
      igraph::intersection(subgraph_to_add, zeros.graph)
    )

    if (n_common_edges > 0) {
      return(NULL)
    }
  }

  return(edges_to_add)
}
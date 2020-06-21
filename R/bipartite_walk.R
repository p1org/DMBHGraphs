#' reverse a walk in a graph
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
#' User input validation
#'
#' @details
#' Functions to run validation checks on user-inputted 
#' graphs. 
#' 
#' @param g igraph graph
#' @param type a string describing the graph being checked,
#' used for error messages
#' @name validate
#' 
#' @return \code{NULL}
NULL


#' @describeIn validate Throws an error if graph is not undirected.
validate_directed <- function(g, type){
  if (!is.directed(g)){
    stop(
      sub("graphtype", type, "graphtype must be a directed graph")
    )
  }
}

#' @describeIn validate Throws an error if graph is not undirected.
validate_undirected <- function(g, type){
  if (is.directed(g)){
    stop(
      sub("graphtype", type, "graphtype must be undirected")
    )
  }
}

#' @describeIn validate Throws an error if graph is not simple.
validate_simple <- function(g, type){
  if (!is.simple(g)){
    stop(
      sub("graphtype", type, "graphtype must be a simple graph.")
    )
  }
}

#' @describeIn validate Throws an error if the graph has
#' reciprocated edges
validate_no_recip <- function(g, type){
  if (reciprocity(g, ignore.loops=TRUE) > 0){
    stop(
      sub("graphtype", type, "Reciprocated edges in directed graph graphtype.")
    )
  }
}

#' @describeIn validate Throws an error if the 
#' undirected skeleton of a directed graph is simple
validate_simple_directed <- function(g, type){
  if(!is.simple(as.undirected(g, mode="each"))){
    stop(
      sub("graphtype", type, "Directed graph graphtype is not simple")
    )
  }
}

#' @describeIn validate Applies validation checks required for 
#' the directed component.
validate_directed_graph_part <- function(g, type){
  validate_directed(g, type)              # check if graph is directed
  validate_no_recip(g, type)              # check for reciprocated edges
  validate_simple_directed(g, type)       # check that undirected skeleton is simple (covers multiple edges in same direction)
}

#' @describeIn validate Applies validation checks required for 
#' the bidirected component.
validate_bidirected_graph_part <- function(g, type){
  validate_undirected(g, type)            # check if graph is undirected
  validate_simple(g, type)                # check if graph is simple
}


#' Balance the number of vertices between two graphs
#' 
#' @details
#' Ensures the input graphs have the same order. If
#' the order is different, vertices are added to the smaller 
#' graph so they have the same number of vertices.
#' 
#' @section: Warnings
#' Raises warning if orders are unbalanced. 
#' 
#' @param g1 igraph graph
#' @param g2 igraph graph
#' 
#' @return List containing the original graphs
#' but augmented with additional vertices if 
#' needed. 
balance_vertices <- function(g1, g2) {
  
  n1 <- igraph::vcount(g1)
  n2 <- igraph::vcount(g2)
  
  if (n1 != n2) {
    warning("Vertex sets unbalanced. Adding vertices to compensate.")
  }
  if (n1 > n2) {
    g2 <- igraph::add_vertices(g2, n1 - n2)
  }
  if (n2 > n1) {
    g1 <- igraph::add_vertices(g1, n2 - n1)
  }
  
  return(list(g1, g2))
}

#' Balance number of vertices in structural zeros graph
#' 
#' @param g igraph graph object, represents structural zeros graph
#' @param n integer, the number of vertices \code{g} should have
#' @param type string, describing the graph 
#' 
#' @return The original graph augmented with additional vertices. 
validate_zeros_graph_order <- function(g, n, type){
  nsz <- igraph::vcount(g)
  
  if (nsz < n){
    # TODO: add warning
    g <- igraph::add_vertices(g, n - nsz)
  } else if (nsz > n){
    stop(sub("graphtype", type, "The inputted structural zeros graphtype graph has more vertices than the inputted graphtype graph."))
  }
  
  return(g)
}
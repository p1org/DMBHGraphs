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

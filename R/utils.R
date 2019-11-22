library(igraph)

# check if graph is directed
validate_directed <- function(g, type){
  if (!is.directed(g)){
    stop(
      sub("graphtype", type, "graphtype must be a directed graph")
    )
  }
}

# check if graph is simple
validate_simple <- function(g, type){
  if (!is.simple(g)){
    stop(
      sub("graphtype", type, "graphtype must be a simple graph.")
    )
  }
}

# check that the graph has no reciprocated edges
validate_no_recip <- function(g, type){
  if (reciprocity(g, ignore.loops=TRUE) > 0){
    stop(
      sub("graphtype", type, "Reciprocated edges in directed graph graphtype.")
    )
  }
}

# check that the undirected skeleton of a directed graph is simple
validate_simple_directed <- function(g, type){
  if(!is.simple(as.undirected(g, mode="each"))){
    stop(
      sub("graphtype", type, "Directed graph graphtype is not simple")
    )
  }
}
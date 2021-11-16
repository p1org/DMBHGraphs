# check if graph is directed
validate_directed <- function(g, type){
  if (!is.directed(g)){
    stop(
      sub("graphtype", type, "graphtype must be a directed graph")
    )
  }
}

# check if graph is undirected
validate_undirected <- function(g, type){
  if (is.directed(g)){
    stop(
      sub("graphtype", type, "graphtype must be undirected")
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

validate_directed_graph_part <- function(g, type){
  validate_directed(g, type)              # check if graph is directed
  validate_no_recip(g, type)              # check for reciprocated edges
  validate_simple_directed(g, type)       # check that undirected skeleton is simple (covers multiple edges in same direction)
}

validate_bidirected_graph_part <- function(g, type){
  validate_undirected(g, type)            # check if graph is undirected
  validate_simple(g, type)                # check if graph is simple
}
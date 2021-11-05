library(igraph)

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



#' Finds reciprocated edges
#' 
#' Finds reciprocated edges by checking if the sending vertex u
#' is in the out-neighborhood of its out-neighbors, i.e. is u in N(v) for v in N(u)
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

#' Splits graph into reciprocated and unreciprocated parts
#' 
#' The graph containing the reciprocated edges is represented by an igraph undirected graph object. 
#' The graph containing the unreciprocated edges is represented by an igraph directed graph. 
#' 
#' @param g igraph directed graph
#' 
#' @return list(igraph undirected graph, igraph directed graph)
split_directed <- function(g) {
  recip_idx <- reciprocated_edges(g)
  return(
    list(
      gudir=igraph::as.undirected(
        igraph::subgraph.edges(g, igraph::E(g)[recip_idx], delete.vertices=FALSE), mode="collapse"),
      gdir=igraph::subgraph.edges(g, igraph::E(g)[!recip_idx], delete.vertices=FALSE)
    )
  )
}
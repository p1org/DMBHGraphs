
sample_edges <- function(g, small.moves.coin = NULL) {

    edges <- igraph::E(g)

    subset_size <- sample(2:igraph::ecount(g), 1)

    if (!is.null(small.moves.coin)) {
        if (runif(1) < small.moves.coin) {
            subset_size <- sample(2:4, 1) # is 4 a hard-coded parameter? 
        }
    }

    edge_sample <- sample(edges, subset_size)
    return(edge_sample)
}

# TODO: find bug in null graph
recursive_partition <- function(edges) {

    n <- length(edges)

    if (n == 2 || n == 3) {
        return(list(edges))
    } else if (n == 4) {

        e1 <- edges[1:2]
        e2 <- edges[3:4]

        return(list(list(e1), list(e2)))
    } else {
        
        j <- sample(3:(n - 1))
        e1 <- edges[1:(j - 1)]
        e2 <- edges[j:n]

        return(list(list(e1), recursive_partition(e2)))
    }
}



# Source: https://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion/8139959#8139959
flatten_list <- function(x) {
  len <- sum(rapply(x, function(x) 1L))
  y <- vector('list', len)
  i <- 0L
  rapply(x, function(x) { i <<- i+1L; y[[i]] <<- x })
  y
}


get_edges_to_add <- function(g, partitions, zeros.graph = NULL) {
  # TODO: do something about memory copy
  # TODO: consider using ... instead of zeros.graph passing
  new_edgelists <- lapply(
      X = partitions,
      FUN = bipartite_walk,
      g = g,
      zeros.graph = zeros.graph)

  new_edges <- lapply(
      X = new_edgelists,
      FUN = igraph::graph_from_edgelist,
      directed = TRUE)

  return(do.call(igraph::union, new_edges))
}

# TODO: validate
check_mutual_edges <- function(gdir, r, b){
    mutual_edge_graph <- igraph::intersection(b, igraph::difference(gdir, r))
    if (igraph::ecount(mutual_edge_graph) > 0){
        return(FALSE)
    } else {
        return(TRUE)
    }
}


validate_new_edges <- function(gdir, gudir, r, b){

    if (!igraph::is.simple(b)){
        return(FALSE)
    }

    if (isFALSE(check_mutual_edges(gdir, r, b))) {
        return(FALSE)
    }
    
    # TODO: add intersection check on undirected component, need algorithm clarification
}
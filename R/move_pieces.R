
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

        edges <- sample(edges, size = length(edges))
        e1 <- edges[1:2]
        e2 <- edges[3:4]

        return(list(list(e1), list(e2)))
    } else {
        
        j <- sample(3:(n - 1), size = 1)
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

    # filter out NULLs 
    new_edgelists <- new_edgelists[!unlist(lapply(new_edgelists, is.null))]
    # if there are no edge sets left, return NULL
    if (length(new_edgelists) == 0) {
        return(NULL)
    }

    # convert edgelists to graphs
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

check_intersection <- function(gudir, b) {

    intersection_graph <- igraph::intersection(
        gudir,
        igraph::as.undirected(b, mode = "collapse")
    )

    if (igraph::ecount(intersection_graph) > 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}


validate_new_edges <- function(gdir, gudir, r, b){

    if (!igraph::is.simple(b)) {
        return(FALSE)
    }

    if (isFALSE(check_mutual_edges(gdir, r, b))) {
        return(FALSE)
    }

    # TODO: need algorithm clarification
    if (isFALSE(check_intersection(gudir, b))) {
        return(FALSE)
    }

    return(TRUE)
}


get_directed_piece <- function(gdir, zeros.graph=NULL, small.moves.coin=0){

    # 1. choose random subset of edges from gdir
    # 2. choose random ordering of the sample
    # 3. choose random composition of the sample
    # 4. run bipartite_walk() on each composition
    # 5. take the union of the edges returned by bipartite_walk()
    # 6. apply rejection / trivial move checks, otherwise return the union as the edges to add
}

sample_edges <- function(g, small.moves.coin = NULL) {

    edges <- igraph::E(g)

    subset_size <- sample(2:igraph::ecount(g), 1)

    if (!is.null(small.moves.coin)) {
        if (runif(1) < small.moves.coin) {
            subset_size <- sample(2:4, 1)
        }
    }

    edge_sample <- sample(edges, subset_size)
    return(edge_sample)
}


# TODO: find some test case for when n > 4
recursive_partition <- function(edges) {

    n <- length(edges)
    edges <- sample(edges, size = length(edges))

    if (n == 2 || n == 3) {
        return(list(edges))
    } else if (n == 4) {
        
        if (runif(n = 1) < 0.5) {
            return(list(edges))
        } else {
            e1 <- edges[1:2]
            e2 <- edges[3:4]
            return(list(list(e1), list(e2)))
        }

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

    new_edgelists <- lapply(
        X = partitions,
        FUN = bipartite_walk,
        g = g,
        zeros.graph = zeros.graph)

    # if any of the results of bipartite_walk are NULL, return NULL
    if (sum(unlist(lapply(new_edgelists, is.null))) > 0) {
        return(NULL)
    }

    # convert edgelists to graphs
    new_edges <- lapply(
        X = new_edgelists,
        FUN = igraph::graph_from_edgelist,
        directed = TRUE)

  return(do.call(igraph::union, new_edges))
}


check_mutual_edges <- function(gdir, r, b) {
    mutual_edge_graph <- igraph::intersection(b, igraph::difference(gdir, r))
    if (igraph::ecount(mutual_edge_graph) > 0) {
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


get_directed_piece <- function(gdir, gudir, zeros.graph = NULL, small.moves.coin = 0) {

    r <- sample_edges(gdir, small.moves.coin = small.moves.coin)
    partitions <- flatten_list(recursive_partition(r))
    b <- get_edges_to_add(gdir, partitions, zeros.graph = zeros.graph)

    if (is.null(b)) {
        return(NULL)
    }
    if (isFALSE(validate_new_edges(gdir, gudir, igraph::graph_from_edgelist(igraph::ends(gdir, r), directed=TRUE), b))) {
        return(NULL) # TODO: what to return for trivial move? 
    } else {
        return(list(r = r, b = igraph::E(b)))
    }
}
#' samples edge set of a graph
#' 
#' Takes a random sample of edges from a graph. Also has an 
#' optional parameter to favor small samples with a specified
#' probability.
#' 
#' This parameter is the small.moves.coin which is a numeric
#' between (0, 1) representing the probability of returning
#' a small sample, of size between 2 and 4. 
#' 
#' @param g igraph graph object
#' @param small.moves.coin numeric in the interval (0, 1)
#' 
#' @return igraph.es
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

#' randomly partition list of edges
#' 
#' @param edges igraph.es object
#' @return list
recursive_partition <- function(edges) {

# TODO: find some test case for when n > 4

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

#' returns graph containing new edges to add
#' 
#' This is effectively a wrapper around bipartite_walk()
#' 
#' Takes a list of edge partitions taken from a graph 
#' along with the graph itself and applies the \code{bipartite_walk}
#' function over each set of edges. 
#' 
#' If none of the results are NULL, the graph whose edges are 
#' the union of all the bipartite walks is returned.
#' 
#' @param g igraph graph
#' @param partitions list of igraph.es objects
#' @param directed should the new edges be directed
#' @param zeros.graph optional, igraph graph object
#' 
#' @return igraph object or NULL
get_edges_to_add <- function(g, partitions, directed, zeros.graph = NULL) {

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
        directed = directed)

  return(do.call(igraph::union, new_edges))
}

#' check that only new edges are added to graph
#' 
#' Check that none of the edges being added are already
#' contained in the graph. 
#' 
#' @param g igraph graph
#' @param r igraph graph containing edges to remove
#' @param b igraph graph containing edges to add
#' 
#' @return boolean
check_mutual_edges <- function(g, r, b) {
    mutual_edge_graph <- igraph::intersection(b, igraph::difference(g, r))
    if (igraph::ecount(mutual_edge_graph) > 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' check the intersection of undirected component and edges to add
#' 
#' Checks that that the intersection between the undirected component 
#' graph and the graph representing the edges to add is empty. The 
#' graph representing these edges is converted to an undirected graph
#' before the intersection is taken. 
#' 
#' @param gudir igraph undirected graph
#' @param b igraph directed graph
#' 
#' @return boolean
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

#' run validation checks 
#' 
#' Runs validations in Algorithm 4 (TODO: reference paper)
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param r igraph directed graph
#' @param b igraph undirected graph
#' 
#' @return boolean
validate_type_2_move <- function(gdir, gudir, r, b){

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

#' generate a Type 2 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param zeros.graph optional, igraph graph (directed or undirected)
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return list or NULL
get_directed_piece <- function(gdir, gudir, zeros.graph = NULL, small.moves.coin = NULL) {

    r <- sample_edges(gdir, small.moves.coin = small.moves.coin)
    partitions <- flatten_list(recursive_partition(r))
    b <- get_edges_to_add(gdir, partitions, directed = TRUE, zeros.graph = zeros.graph)

    if (is.null(b)) {
        return(NULL)
    }
    if (isFALSE(validate_type_2_move(gdir, gudir, igraph::graph_from_edgelist(igraph::ends(gdir, r), directed=TRUE), b))) {
        return(NULL)
    } else {
        return(list(r = r, b = igraph::E(b)))
    }
}


#' validate_type_1_move
#' 
#' Runs validations in Algorithm 3 (TODO: reference paper)
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param r igraph undirected graph
#' @param b igraph undirected graph
#' 
#' @return boolean
validate_type_1_move <- function(gdir, gudir, r, b) {

    if (!igraph::is_simple(b)) {
        return(FALSE)
    }

    if (isFALSE(check_mutual_edges(gudir, r, b))) {
        return(FALSE)
    }

    if (isFALSE(check_intersection(igraph::as.undirected(gdir, mode="collapse"), b))) {
        return(FALSE)
    }

    return(TRUE)
}


#' generate a Type 1 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param zeros.graph optional, igraph graph (directed or undirected)
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return list(r=igraph.graph, b=igraph.graph) or NULL
generate_type_1_move <- function(gdir, gudir, zeros.graph = NULL, small.moves.coin = NULL) {

    directed_skeleton <- igraph::as.directed(gudir, mode = "arbitrary")
    r <- sample_edges(directed_skeleton, small.moves.coin = small.moves.coin)
    partitions <- flatten_list(recursive_partition(r))
    b <- get_edges_to_add(directed_skeleton, partitions, directed = FALSE, zeros.graph = zeros.graph)

    if (is.null(b)) {
        return(NULL)
    }
    
    r <- igraph::graph_from_edgelist(igraph::ends(directed_skeleton, r), directed = FALSE)

    if (isFALSE(validate_type_1_move(gdir, gudir, r, b))) {
        return(NULL)
    } else {
        return(list(r = r, b = b))
    }
}


#' applies a Type 1 move
#' 
#' @param gudir igraph undirected graph
#' @param r igraph undirected graph
#' @param b igraph undirected graph
#' 
#' @return igraph.graph
apply_type_1_move <- function(gudir, r, b) {
    igraph::union(
        igraph::difference(gudir, r), b
    )
}
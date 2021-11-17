#' Sample edges of a graph
#' 
#' Sample edges of a graph with the option to favor small samples.
#' 
#' @param g igraph graph object
#' @param small.moves.coin numeric in the interval (0, 1),
#' controls the probability of choosing a small sample
#' 
#' @return igraph.es
sample_edges <- function(g, small.moves.coin = NULL) {

    m <- igraph::ecount(g) # TODO: handle case where m = 0

    if (m == 1 || m == 2) {
        subset_size <- m
    } else {

        subset_size <- sample(2:m, 1)
        # TODO: need to increase minimum move size because can't have a move with less than 4 vertices
        if (!is.null(small.moves.coin)) {
            if (runif(1) < small.moves.coin) {
                subset_size <- sample(2:min(m, 4), 1)
            }
        }   
    }

    edge_sample <- sample(igraph::E(g), subset_size)
    return(edge_sample)
}

#' randomly partition list of edges
#' 
#' @param edges igraph.es object
#' @return list
recursive_partition <- function(edges) {

# TODO: find some test case for when n > 4

    n <- length(edges)
    edges <- sample(edges, size = n)

    if (n == 1 || n == 2 || n == 3) {
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


#' Computes edges to add to the move
#' 
#' Apply \code{bipartite_walk} to a list of edge partitions.
#' 
#' @details
#' Takes a list of edge partitions taken from a graph 
#' along with the graph itself and applies the \code{bipartite_walk}
#' over each set of edges.
#' 
#' If none of the results are \code{NULL}, the graph whose edges are
#' the union of all the bipartite walks is returned.
#' 
#' @param g igraph graph
#' @param partitions list of igraph.es objects of edges from \code{g}
#' @param directed boolean, should the new edges be directed?
#' 
#' @return igraph object or \code{NULL}
#' 
#' @seealso \code{\link{bipartite_walk}}, \code{\link{recursive_bipartition}}
get_edges_to_add <- function(g, partitions, directed) {

    new_edgelists <- lapply(
        X = partitions,
        FUN = bipartite_walk,
        g = g)

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

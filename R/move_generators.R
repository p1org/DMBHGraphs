# TODO: add @sources to this and other move generators
#' Generate a Type 1 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return 
#' A list with the following elements
#' \itemize{
#'  \item \code{r} edges to remove, an undirected igraph graph
#'  \item \code{b} edges to add, an undirected igraph graph
#' }
#' 
#' Will return \code{NULL} if move is not a simple graph or all partitions are empty. 
#' 
#' @seealso \code{\link{get_edges_to_add}}, \code{\link{sample_edges}}
generate_type_1_move <- function(gdir, gudir, small.moves.coin = NULL) {

    directed_skeleton <- igraph::as.directed(gudir, mode = "arbitrary")
    r <- sample_edges(directed_skeleton, small.moves.coin = small.moves.coin)
    partitions <- flatten_list(recursive_partition(r))
    partitions <- partitions[which(unlist(lapply(X=partitions, FUN=length)) > 1)]
    
    if (identical(partitions, list())) {
        return(NULL)
    }
    b <- get_edges_to_add(directed_skeleton, partitions, directed = FALSE)

    if (is.null(b)) {
        return(NULL)
    }
    
    r <- igraph::graph_from_edgelist(igraph::ends(directed_skeleton, r), directed = FALSE)

    return(list(r = r, b = b))
}


#' Generate a Type 2 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return 
#' A list with the following elements
#' \itemize{
#'  \item \code{r} edges to remove, a directed igraph graph
#'  \item \code{b} edges to add, a directed igraph graph
#' }
#' 
#' Will return \code{NULL} if move is not a simple graph or all partitions are empty. 
#' 
#' @seealso \code{\link{get_edges_to_add}}, \code{\link{sample_edges}}
generate_type_2_move <- function(gdir, gudir, small.moves.coin = NULL) {

    r <- sample_edges(gdir, small.moves.coin = small.moves.coin)
    partitions <- flatten_list(recursive_partition(r))
    partitions <- partitions[which(unlist(lapply(X=partitions, FUN=length)) > 1)]
    
    if (identical(partitions, list())) {
        return(NULL)
    }
    b <- get_edges_to_add(gdir, partitions, directed = TRUE)

    if (is.null(b)) {
        return(NULL)
    }
    r <- igraph::graph_from_edgelist(igraph::ends(gdir, r), directed = TRUE)

    return(list(r = r, b = b))
}

# TODO: add @sources section
#' Generate a Type 3 move
#' 
#' Apply a composition of Type 1 and Type 2 moves
#' 
#' This function generates both Type 1 and Type 2 moves. The Type 1 move is first generated
#' then applied, followed by the Type 2 move to ensure valid moves are not
#' rejected due to the addition of conflicting edges. 
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return
#' A list with the following elements:
#' \itemize{
#'  \item \code{r_u}: undirected igraph graph representing Type 1 move
#'  \item \code{b_u}: undirected igraph graph representing Type 1 move
#'  \item \code{r_d}: directed igraph graph representing Type 2 move
#'  \item \code{b_d}: directed igraph graph representing Type 2 move
#' }
#' 
#' This function will return \code{NULL} a move is a multigraph or all edge partitions are empty. 
#' 
#' @seealso \code{\link{generate_type_1_move}}, \code{\link{generate_type_2_move}}, \code{\link{get_edges_to_add}}
generate_type_3_move <- function(gdir, gudir, small.moves.coin = NULL) {

    type_1_move <- generate_type_1_move(gdir, gudir, small.moves.coin)
    if (is.null(type_1_move)) {
        return(NULL)
    } 

    type_2_move <- generate_type_2_move(gdir, apply_type_1_move(gudir, type_1_move$r, type_1_move$b), small.moves.coin)
    if (is.null(type_2_move)) {
        return(NULL)
    }

    return(
        list(
            r_u = type_1_move$r,
            b_u = type_1_move$b,
            r_d = type_2_move$r,
            b_d = type_2_move$b
        )
    )
}


#' Applies a Type 1 move
#' 
#' @param gudir igraph undirected graph
#' @param r igraph undirected graph
#' @param b igraph undirected graph
#' 
#' @return directed igraph graph
#' 
#' @seealso \code{\link{generate_type_1_move}}
apply_type_1_move <- function(gudir, r, b) {
    igraph::union(
        igraph::difference(gudir, r), b
    )
}


#' Applies a Type 2 move
#' 
#' @param gdir igraph directed graph
#' @param r igraph directed graph
#' @param b igraph directed graph
#' 
#' @return directed igraph graph
#' 
#' @seealso \code{\link{generate_type_2_move}}
apply_type_2_move <- function(gdir, r, b) {
    igraph::union(
        igraph::difference(gdir, r), b
    )
}


#' Applies a Type 3 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param r_u igraph undirected graph
#' @param b_u igraph undirected graph
#' @param r_d igraph directed graph
#' @param b_d igraph directed graph
#' 
#' @return 
#' A list containing the following elements
#' \itemize{
#'  \item \code{gdir}: a directed igraph graph
#'  \item \code{gudir}: an undirected igraph graph
#' }
#' 
#' @seealso \code{link{generate_type_3_move}}
apply_type_3_move <- function(gdir, gudir, r_d, b_r, r_u, b_u) {
    list(
        gdir = apply_type_2_move(gdir, r_d, b_d),
        gudir = apply_type_1_move(gudir, r_u, b_u)
    )
}


#' Generates a move for the p1 model without reciprocation
#' 
#' @details
#' Combines the directed and undirected component graphs into a single
#' directed graph before calling \code{generate_type_2_move}
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin options, numeric between (0,1)
#' 
#' @return list(r = igraph.graph (directed), b = igraph.graph (directed) ) or NULL
#' 
#' @seealso \code{\link{generate_type_2_move}}
generate_p1_wo_recip_move <- function(gdir, gudir, small.moves.coin=NULL) {

    gcomb <- igraph::union(
        igraph::as.directed(gudir, mode = "mutual"), 
        gdir)

    return(generate_type_2_move(gcomb, NULL, small.moves.coin))
}


#' Generates a move for the p1 model with edge-dependent reciprocation
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' @param move.type.coin optional, vector of probability weights of length 3.
#' These control the probability of using a Type 1, 2 or 3 move, respectively. 
#' 
#' @return list containing move components
#' 
#' @seealso \code{link{generate_type_1_move}}, \code{link{generate_type_2_move}}, \code{link{generate_type_3_move}}
generate_p1_ed_recip_move <- function(gdir, gudir, small.moves.coin=NULL, move.type.coin=c(1/3,1/3,1/3)) {
    
    move_type <- sample.int(3, 1, prob = move.type.coin)

    if (move_type == 1) {
        results <- generate_type_1_move(gdir, gudir, small.moves.coin)
    }
    if (move_type == 2) {
        results <- generate_type_2_move(gdir, gudir, small.moves.coin)
    }
    if (move_type == 3) {
        results <- generate_type_3_move(gdir, gudir, small.moves.coin)
    }

    attr(results, "type") <- move_type
    return(results)
}

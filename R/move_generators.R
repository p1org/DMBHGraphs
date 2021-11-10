#' generate a Type 1 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return list(r=igraph.graph, b=igraph.graph) or NULL
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


#' generate a Type 2 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return list(r = igraph.graph (directed), b = igraph.graph (directed) ) or NULL
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


#' generate a Type 3 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param small.moves.coin optional, numeric between (0, 1)
#' 
#' @return list(r_u=igraph.graph (undirected), b_u=igraph.graph (undirected), r_d=igraph.graph (directed), b_d=igraph.graph (directed)) or NULL
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


#' applies a Type 2 move
#' 
#' @param gdir igraph directed graph
#' @param r igraph directed graph
#' @param b igraph directed graph
#' 
#' @return igraph.graph
apply_type_2_move <- function(gdir, r, b) {
    igraph::union(
        igraph::difference(gdir, r), b
    )
}


#' applies a Type 3 move
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param r_u igraph undirected graph
#' @param b_u igraph undirected graph
#' @param r_d igraph directed graph
#' @param b_d igraph directed graph
#' 
#' @return igraph.graph
apply_type_3_move <- function(gdir, gudir, r_d, b_r, r_u, b_u) {
    list(
        gdir = apply_type_2_move(gdir, r_d, b_d),
        gudir = apply_type_1_move(gudir, r_u, b_u)
    )
}


generate_p1_wo_recip_move <- function(gdir, gudir, small.moves.coin=NULL) {

    gcomb <- igraph::union(
        igraph::as.directed(gudir, mode = "mutual"), 
        gdir)

    results <- generate_type_2_move(gcomb, NULL, small.moves.coin)
    return(results)
}


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
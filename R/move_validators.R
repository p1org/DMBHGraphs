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

#' check for bidirected edges
#' 
#' Checks if a directed graph contains bidirected edges. Note
#' that this function will also return true if it contains 
#' multi-edges. 
#' 
#' 
#' @param g igraph directed graph
#'
#' @return boolean
check_bidirected <- function(g) {
    status <- !igraph::is.simple(
        igraph::as.undirected(g, mode = "each")
    )
    return(status)
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


#' validate_type_2_move 
#' 
#' Runs validations in Algorithm 4 (TODO: reference paper)
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @param r igraph directed graph
#' @param b igraph directed graph
#' 
#' @return boolean
validate_type_2_move <- function(gdir, gudir, r, b){

    # checks that no multiedges will be added in the move
    if (!igraph::is.simple(b)) {
        return(FALSE)
    }
    # checks that only new edges will be added in the move
    if (isFALSE(check_mutual_edges(gdir, r, b))) {
        return(FALSE)
    }
    # checks that no bidirected edges will be created in the move
    if (isTRUE(check_bidirected(igraph::union(igraph::difference(gdir, r), b)))) {
        return(FALSE)
    }
    # checks that no edges that are in the undirected graph representing the reciprocated edges
    # are added again during the move (this would create a multiedge in union(gudir, gdir))
    if (isFALSE(check_intersection(gudir, b))) {
        return(FALSE)
    }

    return(TRUE)
}


#' Runs validation for Type 3 move (TODO: reference paper)
#' 
#' Since Type 3 move is a composition of Type 1 and Type 2 moves, 
#' the validation functions for those moves are run separately.
#' 
#' @param b_d igraph directed graph
#' @param b_u igraph undirected graph
#' 
#' @return boolean
validate_type_3_move <- function(b_d, b_u) {

    if (isFALSE(check_intersection(igraph::as.undirected(b_d, mode="collapse"), b_u))) {
        return(FALSE)
    }

    return(TRUE)
}


#' Applies validation check for moves for the p1 model with no reciprocation
#' 
#' @param gcomb igraph directed graph
#' @param r igraph directed graph
#' @param g igraph directed graph
#' 
#' @return boolean
validate_p1_wo_recip_move <- function(gcomb, r, b) {

    # checks that no multiedges will be added in the move
    if (!igraph::is.simple(b)) {
        return(FALSE)
    }

    # checks that only new edges will be added in the move
    if (isFALSE(check_mutual_edges(gcomb, r, b))) {
        return(FALSE)
    }

    return(TRUE)
}


#' Applies validation check for moves for the p1 model with edge-dependent reciprocation
#' 
#' @param gdir igraph directed graph
#' @param gudir igraph undirected graph
#' @moves list
#' 
#' @return boolean
validate_p1_ed_recip_move <- function(gdir, gudir, moves) {
    move_type <- attributes(moves)$type

    if (move_type == 1) {
        return(validate_type_1_move(gdir, gudir, moves$r, moves$b))
    }
    if (move_type == 2) {
        return(validate_type_2_move(gdir, gudir, moves$r, moves$b))
    }
    if (move_type == 3) {

        if (isFALSE(validate_type_1_move(gdir, gudir, moves$r_u, moves$b_u))) {
            return(FALSE)
        }

        if (isFALSE(
                validate_type_2_move(
                    gdir, 
                    apply_type_1_move(gudir, moves$r_u, moves$b_u), 
                    moves$r_d, 
                    moves$b_d)
                )
            ) 
        {
            return(FALSE)
        }

        return(validate_type_3_move(moves$b_d, moves$b_u))
    }
}

testthat::context("Test functions used in the move_pieces.R file")
library(igraph)


######################################
########### sample_edges() ###########
######################################

testthat::test_that(
    "Test that sample_edges returns an igraph.es object", 
    {
        G <- igraph::erdos.renyi.game(n = 10, p.or.m = 15, directed = TRUE, type = "gnm")
        edge_sample <- sample_edges(G)

        testthat::expect_s3_class(edge_sample, "igraph.es")
    }
)


testthat::test_that(
    "Test that sample_edges returns an igraph.es object when small.moves.coin is set", 
    {
        G <- igraph::erdos.renyi.game(n = 10, p.or.m = 15, directed = TRUE, type = "gnm")
        edge_sample <- sample_edges(G, small.moves.coin = 0.99)

        testthat::expect_s3_class(edge_sample, "igraph.es")
    }
)


######################################
####### recursive_partition() ########
######################################

testthat::test_that(
    "Test that recursive_partition returns correct output lengths for n=2,3,4",
    {
        G2 <- igraph::erdos.renyi.game(n = 3, p.or.m = 2, directed = TRUE, type = "gnm")
        G3 <- igraph::erdos.renyi.game(n = 3, p.or.m = 3, directed = TRUE, type = "gnm")
        G4 <- igraph::erdos.renyi.game(n = 4, p.or.m = 4, directed = TRUE, type = "gnm")

        edges2 <- recursive_partition(E(G2))
        edges3 <- recursive_partition(E(G3))
        edges4 <- recursive_partition(E(G4))

        testthat::expect_length(edges2, 1)
        testthat::expect_length(edges3, 1)
        testthat::expect_true(length(edges4) == 2 || length(edges4) == 1)
        testthat::expect_length(edges2[[1]], 2)
        testthat::expect_length(edges3[[1]], 3)
    }
)


######################################
####### check_mutual_edges() #########
######################################

testthat::test_that(
    "check_mutual_edges returns TRUE when expected", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        testthat::expect_true(check_mutual_edges(G, r, b))
    }
)


testthat::test_that(
    "check_mutual_edges returns FALSE when expected", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2),
        c(1, 4)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        testthat::expect_true(!check_mutual_edges(G, r, b))
    }
)


######################################
####### check_intersection() #########
######################################

testthat::test_that(
    "Test that check_intersection returns FALSE when expected", 
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        G2 <- graph_from_edgelist(matrix(c(
            c(2, 1),
            c(4, 2)
        ), ncol = 2, byrow = TRUE))

        testthat::expect_true(!check_intersection(G1, G2))
    }
)


testthat::test_that(
    "Test that check_intersection returns TRUE when expected", 
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        G2 <- graph_from_edgelist(matrix(c(
            c(3, 2),
            c(3, 4)
        ), ncol = 2, byrow = TRUE))

        testthat::expect_true(check_intersection(G1, G2))
    }
)


######################################
######## check_bidirected() ##########
######################################

testthat::test_that(
    "Test that check_bidirected returns FALSE when directed graph contains bidirected edges",
    {
        g <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3),
            c(3, 1)), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- check_bidirected(g)
        testthat::expect_true(result)
    }
)


######################################
###### validate_type_2_move() ########
######################################

testthat::test_that(
    "Check validate_type_2_move() returns FALSE when b is not simple",
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 1),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- validate_type_2_move(NULL, NULL, NULL, G1)

        testthat::expect_true(!result)
    }
)


testthat::test_that(
    "Test that validate_type_2_move() returns FALSE when b adds a bidirected edge to G-r",
    {
        # example graphs taken from check_mutual_edges() test case where TRUE is expected to make sure 
        # the relevant part of the code in validate_type_2_move() is executed
        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 1)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- validate_type_2_move(G, NULL, r, b)
        testthat::expect_false(result)
    }
)


testthat::test_that(
    "Test that validate_type_2_move returns TRUE for valid inputs",
    {
        G_dir <- igraph::graph_from_edgelist(matrix(c(
                c(1,4),
                c(3,2)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        G_udir <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,4)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,4),
                c(3,2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,2),
                c(4,3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- validate_type_2_move(G_dir, G_udir, r, b)
        testthat::expect_true(result)

        }
)

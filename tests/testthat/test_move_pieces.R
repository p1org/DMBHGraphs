testthat::context("Test functions used in the move_pieces.R file")

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

testthat::test_that(
    "Test that sample_edges returns sets of length 1 and 2, resp. when G contains only 1 or 2 edges",
    {
        G1 <- igraph::erdos.renyi.game(n = 10, p.or.m = 1, directed = TRUE, type = "gnm")
        G2 <- igraph::erdos.renyi.game(n = 10, p.or.m = 2, directed = TRUE, type = "gnm")

        edge_sample_1 <- sample_edges(G1)
        edge_sample_2 <- sample_edges(G2)

        testthat::expect_equal(length(edge_sample_1), 1)
        testthat::expect_equal(length(edge_sample_2), 2)
    }
)


######################################
####### recursive_partition() ########
######################################

testthat::test_that(
    "Test that recursive_partition returns correct output lengths for edge-cases",
    {
        G1 <- igraph::erdos.renyi.game(n = 3, p.or.m = 1, directed = TRUE, type = "gnm")
        G2 <- igraph::erdos.renyi.game(n = 3, p.or.m = 2, directed = TRUE, type = "gnm")
        G3 <- igraph::erdos.renyi.game(n = 3, p.or.m = 3, directed = TRUE, type = "gnm")
        G4 <- igraph::erdos.renyi.game(n = 4, p.or.m = 4, directed = TRUE, type = "gnm")

        edges1 <- recursive_partition(E(G1))
        edges2 <- recursive_partition(E(G2))
        edges3 <- recursive_partition(E(G3))
        edges4 <- recursive_partition(E(G4))

        testthat::expect_length(edges1, 1)
        testthat::expect_length(edges2, 1)
        testthat::expect_length(edges3, 1)
        testthat::expect_true(length(edges4) == 2 || length(edges4) == 1)
        testthat::expect_length(edges1[[1]], 1)
        testthat::expect_length(edges2[[1]], 2)
        testthat::expect_length(edges3[[1]], 3)
    }
)

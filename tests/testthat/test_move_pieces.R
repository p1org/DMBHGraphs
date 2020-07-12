testthat::context("Test functions used in the move_pieces.R file")
library(igraph)




testthat::test_that(
    "Test that sample_edges returns an igraph.es object", 
    {
        G <- igraph::erdos.renyi.game(n = 10, p.or.m = 15, directed = TRUE, type = "gnm")
        edge_sample <- sample_edges(G)

        testthat::expect_s3_class(edge_sample, "igraph.es")
    }
)

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
        testthat::expect_length(edges4, 2)
        testthat::expect_length(edges2[[1]], 2)
        testthat::expect_length(edges3[[1]], 3)
        testthat::expect_length(edges4[[1]][[1]], 2)
        testthat::expect_length(edges4[[2]][[1]], 2)
    }
)


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
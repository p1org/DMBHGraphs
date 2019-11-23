testthat::context("Test Get.MLE functions and helpers")
library(igraph)

testthat::test_that(
  "Test that balance_vertices() function works as expected", {
    
    g1 <- igraph::make_full_graph(3)
    g2 <- igraph::make_full_graph(4)
    
    output <- balance_vertices(g1, g2)
    
    testthat::expect_equal(igraph::vcount(output[[1]]), 4)
    testthat::expect_equal(igraph::vcount(output[[2]]), 4)
    
  }
)
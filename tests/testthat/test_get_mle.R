testthat::context("Test Get.MLE functions and helpers")
library(igraph)

testthat::test_that(
  "Test that balance_vertices() function works as expected", {
    
    g1 <- igraph::make_full_graph(3)
    g2 <- igraph::make_full_graph(4)
    
    testthat::expect_warning(output<-balance_vertices(g1, g2))
    testthat::expect_equal(igraph::vcount(output[[1]]), 4)
    testthat::expect_equal(igraph::vcount(output[[2]]), 4)
    
  }
)

testthat::test_that(
  "Test that validate_structural_zeros_graph() function works as expected", {
    
    n <- 5
    g1 <- igraph::make_full_graph(n-1)
    g2 <- igraph::make_full_graph(n+1)
    
    testthat::expect_equal(igraph::vcount(validate_structural_zeros_graph(g1, n, "test_graph")), 5)
    testthat::expect_error(validate_structural_zeros_graph(g2, n, "test_graph"))
    
    
  }
)
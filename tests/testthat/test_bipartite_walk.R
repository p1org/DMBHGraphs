testthat::context("Test Bipartite.Walk function family")
library(igraph)


testthat::test_that(
  "Test that reversed_walk() function is working correctly",
  {
    
    edges <- matrix(c(
      c(1,3),
      c(2,4),
      c(3,5),
      c(4,6)
    ), nrow=4, byrow=TRUE)
    
    expected_result <- matrix(c(
      c(2,3),
      c(3,4),
      c(4,5),
      c(1,6)
    ), nrow=4, byrow=TRUE)
    
    result <- reverse_walk(edges)
    
    expect_array_equal(expected_result, result)
  }
)

testthat::test_that(
  "Test that bipartite_walk returns the reversed walk when there are no zeros and the graph is simple."
  {
    edges <- matrix(c(
      c(1,3),
      c(2,4),
      c(3,5),
      c(4,6)
    ), nrow=4, byrow=TRUE)
    
    expected_result <- matrix(c(
      c(2,3),
      c(3,4),
      c(4,5),
      c(1,6)
    ), nrow=4, byrow=TRUE)
    
    result <- bipartite_walk(edges)
    
    expect_array_equal(expected_result, result)
  }
)
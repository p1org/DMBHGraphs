testthat::context("Test bipartite_walk function family")
library(igraph)



testthat::test_that(
  "Test that bipartite_walk returns the reversed walk when there are no zeros and the graph is simple.",
  {
    g <- igraph::graph_from_edgelist(matrix(c(
      c(1, 3),
      c(2, 4),
      c(3, 5),
      c(4, 6)
    ), nrow = 4, byrow = TRUE), directed = TRUE)
    
    expected_result <- matrix(c(
      c(2, 3),
      c(3, 4),
      c(4, 5), 
      c(1, 6)
    ), nrow = 4, byrow = TRUE)
    

    result <- bipartite_walk(g, igraph::E(g))
    expect_array_equal(expected_result, result)
  }
)

testthat::test_that(
  "Test that bipartite_walk returns NULL when it tries to add a forbidden edge when zeros graph is undirected",
  {

    b <- igraph::graph_from_edgelist(
    matrix(c(
      c(2, 3),
      c(2, 4)
    ), ncol = 2, byrow = TRUE), directed = TRUE)

    zeros.graph <- igraph::graph_from_edgelist(
     matrix(c(
      c(2, 3),
      c(3, 4),
      c(4, 5),
      c(1, 6)), ncol = 2, byrow = TRUE), directed = FALSE
    )

    result <- check_structural_zeros(b = b, zeros.graph = zeros.graph)
    testthat::expect_false(result)
  }
)


testthat::test_that(
  "Test that check_structural_zeros() returns FALSE when it tries to add a forbidden edge when zeros graph is directed",
  {
    b <- igraph::graph_from_edgelist(
    matrix(c(
      c(2, 3),
      c(2, 4)
    ), ncol = 2, byrow = TRUE), directed = TRUE)

    zeros.graph <- igraph::graph_from_edgelist(
     matrix(c(
      c(2, 3),
      c(3, 4),
      c(4, 5),
      c(1, 6)), ncol = 2, byrow = TRUE)
    )

    result <- check_structural_zeros(b = b, zeros.graph = zeros.graph)
    testthat::expect_false(result)
  }
)


testthat::test_that(
  "Test that bipartite_walk returns NULL when resulting subgraph is not simple",
  {
    g <- igraph::graph_from_edgelist(
    matrix(c(
      c(1, 3),
      c(2, 4),
      c(4, 2),
      c(3, 5),
      c(4, 6)
    ), nrow = 5, byrow = TRUE), directed = TRUE)

    result <- bipartite_walk(g, igraph::E(g))
    testthat::expect_null(result)
  }
)

testthat::context("Test input validation functions")
library(igraph)


testthat::test_that(
  "Test that validate_directed() throws error when passed undirected graph and returns NULL when passed directed graph", {
    
    el <- matrix(
      c(
        c(1,2),
        c(2,3),
        c(3,1)
      ), 
      nc=2,
      byrow=TRUE
    )
    G_undirected <- igraph::graph_from_edgelist(el=el, directed=FALSE)
    G_directed <- igraph::graph_from_edgelist(el=el, directed=TRUE)
    
    testthat::expect_null(validate_directed(G_directed, type="test_graph"))
    testthat::expect_error(validate_directed(G_undirected, type="test_graph"))
    
  }
)

testthat::test_that(
  "Test that validate_undirected() throws error when passed directed graph and returns NULL when passed undirected graph", {
    
    el <- matrix(
      c(
        c(1,2),
        c(2,3),
        c(3,1)
      ), 
      nc=2,
      byrow=TRUE
    )
    G_empty_dir <- graph.empty(3, directed=TRUE)
    G_empty_udir <- graph.empty(3, directed=FALSE)
    G_undirected <- igraph::graph_from_edgelist(el=el, directed=FALSE)
    G_directed <- igraph::graph_from_edgelist(el=el, directed=TRUE)
    
    testthat::expect_error(validate_undirected(G_directed, type="test_graph"))
    testthat::expect_null(validate_undirected(G_undirected, type="test_graph"))
    testthat::expect_error(validate_undirected(G_empty_dir, type="empty_test_graph"))
    testthat::expect_null(validate_undirected(G_empty_udir, type="empty_test_graph"))
    
    
  }
)

testthat::test_that(
  "Test that validate_simple() throws error for non-simple graphs and returns NULL when passed simple graph", {
    
    el_no_simple <- matrix(
      c(
        c(1,2),
        c(1,2),
        c(2,3),
        c(3,1)
      ),
      nc=2,
      byrow=TRUE
    )
    el_simple <- matrix(
      c(
        c(1,2),
        c(2,3),
        c(3,1)
      ),
      nc=2,
      byrow=TRUE
    )
    G_no_simple_dir <- igraph::graph_from_edgelist(el_no_simple, directed=TRUE)
    G_no_simple_udir <- igraph::graph_from_edgelist(el_no_simple, directed=FALSE)
    G_simple_dir <- igraph::graph_from_edgelist(el_simple, directed=TRUE)
    G_simple_udir <- igraph::graph_from_edgelist(el_simple, directed=FALSE)
    G_empty_udir <- graph.empty(3, directed=FALSE)
    
    testthat::expect_error(validate_simple(G_no_simple_dir, "test_graph"))
    testthat::expect_error(validate_simple(G_no_simple_udir, "test_graph"))
    testthat::expect_null(validate_simple(G_simple_dir), "test_graph")
    testthat::expect_null(validate_simple(G_simple_udir), "test_graph")
    testthat::expect_null(validate_simple(G_empty_udir), "test_empty_graph")
    
  }
)

testthat::test_that(
  "Test that validate_no_recip() throws error for graphs with reciprocated edges and NULL with no reciprocated edges", {
    
    el_no_recip <- matrix(
      c(
        c(1,2),
        c(2,3)
      ),
      nc=2,
      byrow=TRUE
    )
    el_recip <- matrix(
      c(
        c(1,2),
        c(2,1)
      ),
      nc=2,
      byrow=TRUE
    )
    
    G_no_recip <- igraph::graph_from_edgelist(el_no_recip, directed=TRUE)
    G_recip <- igraph::graph_from_edgelist(el_recip, directed=TRUE)
    
    testthat::expect_error(validate_no_recip(G_recip, "test_graph"))
    testthat::expect_null(validate_no_recip(G_no_recip, "test_graph"))
  }
)

testthat::test_that(
  "Test that validate_simple_directed() throws an error when the undirected skeleton of a directed graph is not simple and NULL otherwise", {
    
    el_no_simple <- matrix(
      c(
        c(1,2),
        c(1,2),
        c(2,3),
        c(3,1)
      ),
      nc=2,
      byrow=TRUE
    )
    el_simple <- matrix(
      c(
        c(1,2),
        c(2,3),
        c(3,1)
      ),
      nc=2,
      byrow=TRUE
    )
    
    G_no_simple_dir <- igraph::graph_from_edgelist(el_no_simple, directed=TRUE)
    G_simple_dir <- igraph::graph_from_edgelist(el_simple, directed=TRUE)
    
    testthat::expect_error(validate_simple_directed(G_no_simple_dir, "test_graph"))
    testthat::expect_null(validate_simple_directed(G_simple_dir), "test_graph")
  }
)
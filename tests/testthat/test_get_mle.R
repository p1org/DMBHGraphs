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


testthat::test_that(
  "Test that structural zeros initialization matrix is correct", {
    
    ## create test graph for directed graph
    E_dir <- matrix(c(3,1,2,4,4,2), nc=2, byrow=TRUE)
    gdir <- igraph::graph_from_edgelist(E_dir, directed=TRUE)
    ## create test graph for bidirected graph
    gbidir <- igraph::make_empty_graph(4, directed=FALSE)
    gbidir <- igraph::add_edges(gbidir, c(2,4))
    gbidir <- igraph::add_edges(gbidir, c(3,2))
    
    
    ## Create expected result for basic test case ##
    expected_result <- array(data=1, dim=c(4,4,2,2))
    cells <- list(
      c(1,3,2,1),
      c(3,1,1,2),
      c(3,2,2,2),
      c(2,3,2,2),
      c(2,4,2,2),
      c(4,2,2,2),
      c(2,4,1,2),
      c(2,4,2,1),
      c(4,2,1,2),
      c(4,2,2,1),
      c(4,2,1,1),
      c(2,4,1,1)
    )
    for (c in cells){
      expected_result[c[1],c[2],c[3],c[4]] <- 0
    }
    for (i in 1:4){
      expected_result[i,i,,] <- c(0,0,0,0)
    }
    
    result <- structural_zeros_matrix(gdir, gbidir)
    expect_array_equal(result, expected_result)
  }
)
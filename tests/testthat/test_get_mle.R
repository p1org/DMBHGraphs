testthat::context("Test Get.MLE functions and helpers")

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


testthat::test_that(
  "Test that default_beta_sbm_zeros() is working correctly", {
    
    n <- 4
    blocks <- c(1,1,2,2)
    k <- 2
    # insert test code here
    
    ## Create expected result for basic test case ##
    expected_result <- array(data=1, dim=c(n,n,k+choose(k,2),2))
    
    for (i in 1:n){
      expected_result[i,i, ,c(1,2)] <- 0
    }
    
    # inside block 1
    expected_result[1,2, setdiff(seq(1, k+choose(k,2)), 1), c(1,2)] <- 0
    expected_result[2,1, setdiff(seq(1, k+choose(k,2)), 1), c(1,2)] <- 0
    
    # inside block 2
    expected_result[3,4, setdiff(seq(1, k+choose(k,2)), 2), c(1,2)] <- 0
    expected_result[4,3, setdiff(seq(1, k+choose(k,2)), 2), c(1,2)] <- 0
    
    # between 1 and 2
    expected_result[1,3, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[3,1, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[1,4, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[4,1, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[2,3, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[3,2, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[2,4, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    expected_result[4,2, setdiff(seq(1, k+choose(k,2)), 3), c(1,2)] <- 0
    
    
    result <- default_beta_sbm_zeros(n, blocks)
    expect_array_equal(result, expected_result)

  }
)


testthat::test_that(
  "Test that function for user-defined structural zeros for beta model is working", {
    
    g <- igraph::graph.empty(n=4, directed=FALSE)
    g <- igraph::add.edges(graph=g, edges=c(1,2,1,3))
    
    n <- 4
    k <- 2
    blocks <- c(1,1,2,2)
    
    expected_result <- array(data=1, dim=c(n,n,k+choose(k,2),2))
    
    expected_result[1,2,1,c(1,2)] <- 0
    expected_result[2,1,1,c(1,2)] <- 0
    
    expected_result[1,3,,c(1,2)] <- 0
    expected_result[3,1,,c(1,2)] <- 0
    
    result <- user_defined_beta_sbm_zeros(g, blocks)
    expect_array_equal(result, expected_result)
    
  }
)


testthat::test_that(
  "Test that error checking in Get.MLE.beta.SBM() function is working correctly", {
    
    # set up test fixtures
    gdir <- igraph::graph.empty(n=4, directed=TRUE)
    g<- igraph::graph.empty(n=4)

    # test that error is raised when directed graph is passed
    testthat::expect_error(Get.MLE.beta.SBM(gdir, c(1,2,3,4)))
    
    # test that error is raised when non-consecutive block sequence is passed
    testthat::expect_error(Get.MLE.beta.SBM(g, c(1,3,4,5)))
    
    # test that error is raised when block sequence isn't the same length as the number of vertices
    testthat::expect_error(Get.MLE.beta.SBM(g, c(1,2,3)))
    
  }
)


testthat::test_that(
  "Test that Get.MLE.beta.SBM() executes a complete run successfully on legitimate data.", {
    
    E <- matrix(c(1,2,3,1,4,2), nc=2, byrow=TRUE)
    g <- igraph::graph_from_edgelist(E, directed=FALSE)
    blocks <- c(1,1,2,2)
    k <- 2
    
    user_defined_zeros <- igraph::graph.empty(n=4, directed=FALSE)

    output <- Get.MLE.beta.SBM(g, blocks)
    
    # test that the output dimensions are equal to the dimensions of the configuration matrix
    testthat::expect_equal(dim(output), c(4,4,k+choose(k,2),2))
    
  }
)


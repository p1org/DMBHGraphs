testthat::context("Test Get.Configuration.Matrix function family")
library(igraph)


expect_array_equal <- function(object, expected){

  format_indices <- function(x){
    formatted_indices <- list()
    for (i in 1:dim(x)[1]){
      formatted_indices[[i]] <- paste0("(", paste(x[i,], collapse=","), ")")
    }
    return(formatted_indices)
  }

  act <- testthat::quasi_label(rlang::enquo(object))

  non_zero_indices <- which(object - expected != 0, arr.ind=TRUE)

  status <- TRUE
  test_message <- ""

  if (length(non_zero_indices) > 0){
    status <- FALSE
    test_message <- paste("Nonzero index", format_indices(non_zero_indices), collapse=": ")
  }

  testthat::expect(ok=status, failure_message=test_message)

  invisible(act$val)
}


testthat::test_that(
  "Test that Get.Configuration.Matrix.p1.FW returns correct matrix on toy example", {

    ## Create graph for basic test case ##
    E <- matrix(c(1,2,4,2,1,3,3,1), nc=2, byrow=TRUE)
    G <- igraph::graph_from_edgelist(E, directed=TRUE)

    mixed.graph = split.Directed.Graph(G)
    gdir = mixed.graph[[1]]
    gbidir = mixed.graph[[2]]

    ## Create expected result for basic test case ##
    expected_result <- array(data=0, dim=c(4,4,2,2))
    cells <- list(
      c(1,2,2,1), # 1 -> 2
      c(2,1,1,2), # 2 <- 1
      c(2,4,1,2), # 2 <- 4
      c(4,2,2,1), # 4 -> 2
      c(1,3,2,2), # 1 <-> 3
      c(3,1,2,2), # 3 <-> 1
      c(1,4,1,1), # 1 -x- 4
      c(4,1,1,1), # 4 -x- 1
      c(2,3,1,1), # 2 -x- 3
      c(3,2,1,1), # 3 -x- 2
      c(3,4,1,1), # 3 -x- 4
      c(4,3,1,1)  # 4 -x- 3
    )
    for (c in cells){
      expected_result[c[1],c[2],c[3],c[4]] <- 1
    }

    expect_array_equal(Get.Configuration.Matrix.p1.FW(gdir, gbidir), expected_result)
  }
)

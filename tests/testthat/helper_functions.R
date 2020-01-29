expect_array_equal <- function(object, expected){
  
  format_indices <- function(x){
    # helper function that formats matrix indices for printing,
    # indices contained along rows of matrix
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

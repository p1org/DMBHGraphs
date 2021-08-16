testthat::context("Testing files in test_utils.R other than input validation functions")


testthat::test_that(
    "Test that reciprocated_edges() returns expected output", 
    {
        g <- igraph::graph_from_edgelist(
            matrix(
                c(
                    c(2, 1),
                    c(1, 3),
                    c(2, 3),
                    c(3, 4),
                    c(4, 3),
                    c(4, 5),
                    c(4, 5),
                    c(5, 4)
                ), ncol=2, byrow=TRUE)
        )

        expected_result <- c(rep(FALSE, 3), rep(TRUE, 5))
        result <- reciprocated_edges(g)
        expect_array_equal(expected_result, result)

    }
)
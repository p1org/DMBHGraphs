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


testthat::test_that(
    "Test that split_directed() returns expected output",
    {
        G <- igraph::graph_from_edgelist(
            matrix(
            c(
                c(1,3),
                c(2,1),
                c(3,2),
                c(3,4),
                c(4,3),
                c(4,5),
                c(4,5),
                c(5,4)), ncol=2, byrow=TRUE), directed=TRUE)


        gdir <- add_edges(igraph::make_empty_graph(5), c(2,1,1,3,3,2))
        gudir <- add_edges(igraph::make_empty_graph(5, directed=FALSE), c(3,4,4,5))

        result <- split_directed(G)

        testthat::expect_true(igraph::isomorphic(result$gdir, gdir))
        testthat::expect_true(igraph::isomorphic(result$gudir, gudir))

    }
)
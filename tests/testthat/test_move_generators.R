testthat::context("Test functions used in the move_generators.R file")

######################################
###### generate_type_1_move() ########
######################################

testthat::test_that(
    "Test that generate_type_1_move() returns correct output for known example.",
    {
        gudir <- igraph::graph_from_edgelist(
            matrix(c(
                c(2, 6),
                c(3, 5)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        gdir <- igraph::graph_from_edgelist(
            matrix(c(
                c(1, 6),
                c(1, 2),
                c(2, 6)
                #c(2, 5),
                #c(4, 3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        b_expected <- igraph::graph_from_edgelist(
            matrix(c(
                c(2, 5),
                c(3, 6)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        set.seed(1234)
        result <- generate_type_1_move(gdir, gudir, NULL)

        testthat::expect_equal(sum(degree(b_expected) - degree(result$b)), 0)
    }
)


testthat::test_that(
    "Thest that generate_type_1_move() returns NULL when partitions contain a single edge",
    {

        gudir <- igraph::erdos.renyi.game(n = 3, p.or.m = 1, directed = FALSE, type = "gnm")

        result <- generate_type_1_move(NULL, gudir, NULL)
        testthat::expect_null(result)
    }
)



######################################
###### generate_type_2_move() ########
######################################

testthat::test_that(
    "Thest that generate_type_2_move() returns correct outputs for known valid move",
    {
        gudir <- igraph::graph_from_edgelist(
            matrix(c(
                c(2, 6),
                c(3, 5)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        gdir <- igraph::graph_from_edgelist(
            matrix(c(
                c(1, 6),
                c(1, 2),
                c(2, 6),
                c(2, 5),
                c(4, 3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        b_expected <- igraph::graph_from_edgelist(
            matrix(c(
                c(1, 3),
                c(4, 6)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        set.seed(42)

        result <- generate_type_2_move(gdir, gudir, NULL)
        result_move <- igraph::union(
            igraph::difference(gdir, result$r), result$b
        )

        testthat::expect_true(sum(degree(b_expected) - degree(result$b)) == 0)
        testthat::expect_true(sum(degree(gdir) - degree(result_move)) == 0)

        set.seed(NULL)
    }
)

testthat::test_that(
    "Thest that generate_type_2_move() returns NULL when partitions contain a single edge",
    {

        gdir <- igraph::erdos.renyi.game(n = 3, p.or.m = 1, directed = TRUE, type = "gnm")

        result <- generate_type_2_move(gdir, NULL, NULL)
        testthat::expect_null(result)
    }
)



######################################
###### generate_type_3_move() ########
######################################

testthat::test_that(
    "Test that generate_type_3_move() returns correct output on valid example",
    {

        gdir <- igraph::graph_from_edgelist(
        matrix(c(
            c(1, 3),
            c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        gudir <- igraph::graph_from_edgelist(
        matrix(c(
            c(1, 2),
            c(3, 4)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        r_d <- gdir
        r_u <- gudir

        b_d <- igraph::graph_from_edgelist(
        matrix(c(
            c(1,2),
            c(4,3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b_u <- igraph::graph_from_edgelist(
        matrix(c(
            c(1,4),
            c(2,3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        set.seed(1)
        results <- generate_type_3_move(gdir, gudir, NULL)

        testthat::expect_true(igraph::isomorphic(results$b_d, b_d))
        testthat::expect_true(igraph::isomorphic(results$b_u, b_u))
        set.seed(NULL)
    }
)



######################################
###### apply_type_1_move() ###########
######################################

testthat::test_that(
    "Test that apply_type_1_move() works as expected",
    {
        gudir <- igraph::graph_from_edgelist(
            matrix(c(
                c(4, 1),
                c(1, 3),
                c(2, 3)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- igraph::graph_from_edgelist(
            matrix(c(
                c(4,1),
                c(2,3)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        b <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,1),
                c(4,3)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        expected <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,1),
                c(4,3),
                c(1,3)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- apply_type_1_move(gudir, r, b)
        testthat::expect_true(igraph::isomorphic(expected, result)) 
    }
)



######################################
###### apply_type_2_move() ###########
######################################

testthat::test_that(
    "Test that apply_type_2_move() works as expected",
    {
        gdir <- igraph::graph_from_edgelist(
            matrix(c(
                c(4, 1),
                c(1, 3),
                c(2, 3)

            ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(
            matrix(c(
                c(4,1),
                c(2,3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,1),
                c(4,3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        expected <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,1),
                c(4,3),
                c(1,3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- apply_type_2_move(gdir, r, b)
        testthat::expect_true(igraph::isomorphic(expected, result)) 
    }
)



###########################################
###### generate_p1_wo_recip_move() ########
###########################################

testthat::test_that(
    "Test that generate_p1_wo_recip_move() returns valid output for a known example", 
    {
        gudir <- igraph::graph_from_edgelist(
        matrix(
            c(
             c(2,1),
             c(3,4)
            )
        , ncol=2, byrow=TRUE),
        directed=FALSE
        )

        gdir <- igraph::graph_from_edgelist(
        matrix(
            c(
             c(3,1),
             c(2,4)
            ), ncol=2, byrow=TRUE
        ), directed=TRUE)

        b_expected <- igraph::graph_from_edgelist(
            matrix(c(
                c(3,1),
                c(3,2),
                c(2,4),
                c(1,4)
            ), ncol = 2, byrow = TRUE), directed = TRUE)


        set.seed(888186)

        result <- generate_p1_wo_recip_move(gdir, gudir, NULL)
        testthat::expect_equal(sum(degree(b_expected) - degree(result$b)), 0)
        testthat::expect_true(igraph::isomorphic(result$b, b_expected))
        
        set.seed(NULL)
    }
)
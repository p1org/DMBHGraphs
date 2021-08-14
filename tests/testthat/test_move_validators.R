testthat::context("Test functions used in the move_validators.R file")

######################################
####### check_mutual_edges() #########
######################################

testthat::test_that(
    "check_mutual_edges returns TRUE when expected with directed graphs.", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        testthat::expect_true(check_mutual_edges(G, r, b))
    }
)


testthat::test_that(
    "check_mutual_edges returns FALSE when expected with directed graphs.", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2),
        c(1, 4)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        testthat::expect_false(check_mutual_edges(G, r, b))
    }
)


testthat::test_that(
    "check_mutual_edges returns TRUE when expected on an undirected graph", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        testthat::expect_true(check_mutual_edges(G, r, b))
    }
)


testthat::test_that(
    "check_mutual_edges returns FALSE when expected on undirected graphs", 
    {

        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 2),
        c(1, 4)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        testthat::expect_true(!check_mutual_edges(G, r, b))
    }
)


######################################
####### check_intersection() #########
######################################

testthat::test_that(
    "Test that check_intersection returns FALSE when expected", 
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        G2 <- graph_from_edgelist(matrix(c(
            c(2, 1),
            c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        testthat::expect_true(!check_intersection(G1, G2))
    }
)


testthat::test_that(
    "Test that check_intersection returns TRUE when expected", 
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        G2 <- graph_from_edgelist(matrix(c(
            c(3, 2),
            c(3, 4)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        testthat::expect_true(check_intersection(G1, G2))
    }
)


######################################
######## check_bidirected() ##########
######################################

testthat::test_that(
    "Test that check_bidirected returns TRUE when directed graph contains bidirected edges",
    {
        g <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3),
            c(3, 1)), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- check_bidirected(g)
        testthat::expect_true(result)
    }
)



######################################
###### validate_type_1_move() ########
######################################

testthat::test_that(
    "Check validate_type_1_move() returns FALSE when b is not simple",
    {
        b <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- validate_type_1_move(NULL, NULL, NULL, b)
      
        testthat::expect_false(result)
     }
)

testthat::test_that(
    "Check validate_type_1_move() returns TRUE on valid example",
    {
        Gdir <- igraph::graph_from_edgelist(
            matrix(c(
            c(4,2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        Gudir <- igraph::graph_from_edgelist(
            matrix(c(
            c(1,4),
            c(2,3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- Gudir
        
        b <- igraph::graph_from_edgelist(
            matrix(c(
            c(1,2),
            c(3,4)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- validate_type_1_move(Gdir, Gudir, r, b)

        testthat::expect_true(result)
    }
)


testthat::test_that(
    "Check validate_type_1_move() returns FALSE when b adds edge to directed component",
    {
        Gdir <- igraph::graph_from_edgelist(
            matrix(c(
            c(1,3),
            c(1,4)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        Gudir <- igraph::graph_from_edgelist(
            matrix(c(
            c(1,2),
            c(2,3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- Gudir
        
        b <- igraph::graph_from_edgelist(
            matrix(c(
            c(1,3),
            c(3,4)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- validate_type_1_move(Gdir, Gudir, r, b)

        testthat::expect_false(result)
    }
)



######################################
###### validate_type_2_move() ########
######################################

testthat::test_that(
    "Check validate_type_2_move() returns FALSE when b is not simple",
    {
        G1 <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 1),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = FALSE)

        result <- validate_type_2_move(NULL, NULL, NULL, G1)

        testthat::expect_false(result)
    }
)


testthat::test_that(
    "Test that validate_type_2_move() returns FALSE when b adds a bidirected edge to G-r",
    {
        # example graphs taken from check_mutual_edges() test case where TRUE is expected to make sure 
        # the relevant part of the code in validate_type_2_move() is executed
        G <- igraph::graph_from_edgelist(
            matrix(c(
            c(1, 4),
            c(4, 2),
            c(2, 1),
            c(1, 3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        r <- igraph::graph_from_edgelist(matrix(c(
        c(2, 1),
        c(4, 2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(matrix(c(
        c(2, 3),
        c(4, 1)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- validate_type_2_move(G, NULL, r, b)

        testthat::expect_false(result)
    }
)

testthat::test_that(
    "Test that validate_type_2_move returns TRUE for valid inputs",
    {
        G_dir <- igraph::graph_from_edgelist(matrix(c(
                c(1,4),
                c(3,2)
            ), ncol = 2, byrow = TRUE), directed = TRUE)

        G_udir <- igraph::graph_from_edgelist(
            matrix(c(
                c(2,4)
            ), ncol = 2, byrow = TRUE), directed = FALSE)

        r <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,4),
                c(3,2)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        b <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,2),
                c(4,3)
        ), ncol = 2, byrow = TRUE), directed = TRUE)

        result <- validate_type_2_move(G_dir, G_udir, r, b)
        testthat::expect_true(result)

    }
)


######################################
###### validate_type_3_move() ########
######################################

testthat::test_that(
    "Test that validate_type_3_move() returns FALSE when there is a non-empty intersection",
    {
        b_d <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,2),
                c(4,3),
                c(2,3)
            ), ncol = 2, byrow = TRUE), directed = TRUE)
        
        b_u <- igraph::graph_from_edgelist(
            matrix(c(
                c(1,3),
                c(2,3)
            ), ncol = 2, byrow = TRUE), directed = FALSE)
        
        result <- validate_type_3_move(b_d, b_u)
        testthat::expect_false(result)
    }
)

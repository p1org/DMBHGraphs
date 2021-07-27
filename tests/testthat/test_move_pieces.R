testthat::context("Test functions used in the move_pieces.R file")
library(igraph)




testthat::test_that(
    "Test that sample_edges returns an igraph.es object", 
    {
        G <- igraph::erdos.renyi.game(n = 10, p.or.m = 15, directed = TRUE, type = "gnm")
        edge_sample <- sample_edges(G)

        testthat::expect_s3_class(edge_sample, "igraph.es")
    }
)


testthat::test_that(
    "Test that sample_edges returns an igraph.es object when small.moves.coin is set", 
    {
        G <- igraph::erdos.renyi.game(n = 10, p.or.m = 15, directed = TRUE, type = "gnm")
        edge_sample <- sample_edges(G, small.moves.coin = 0.99)

        testthat::expect_s3_class(edge_sample, "igraph.es")
    }
)


testthat::test_that(
    "Test that recursive_partition returns correct output lengths for n=2,3,4",
    {
        G2 <- igraph::erdos.renyi.game(n = 3, p.or.m = 2, directed = TRUE, type = "gnm")
        G3 <- igraph::erdos.renyi.game(n = 3, p.or.m = 3, directed = TRUE, type = "gnm")
        G4 <- igraph::erdos.renyi.game(n = 4, p.or.m = 4, directed = TRUE, type = "gnm")

        edges2 <- recursive_partition(E(G2))
        edges3 <- recursive_partition(E(G3))
        edges4 <- recursive_partition(E(G4))

        testthat::expect_length(edges2, 1)
        testthat::expect_length(edges3, 1)
        testthat::expect_true(length(edges4) == 2 || length(edges4) == 1)
        testthat::expect_length(edges2[[1]], 2)
        testthat::expect_length(edges3[[1]], 3)
    }
)

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

        testthat::expect_true(!check_mutual_edges(G, r, b))
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

        testthat::expect_true(!result)
    }
)


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
        result <- generate_type_1_move(gdir, gudir, NULL, NULL)

        result_move <- igraph::union(igraph::difference(gudir, result$r),result$b)

        testthat::expect_true(sum(degree(b_expected) - degree(result$b)) == 0)
    }
)

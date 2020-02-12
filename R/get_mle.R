library(igraph)


balance_vertices <- function(g1, g2, message="Warning! Vertex sets unbalanced. Adding vertices to compensate."){
  
  n1 <- igraph::vcount(g1)
  n2 <- igraph::vcount(g2)
  
  if (n1 != n2){
    warning(message)
  }
  if (n1 > n2){
    g2 <- igraph::add_vertices(g2, n1-n2)
  }
  if (n2 > n1){
    g1 <- igraph::add.vertices(g1, n2-n1)
  }
  
  return(list(g1, g2))
}

# TODO: rename this function to clarify what its validating
validate_structural_zeros_graph <- function(g, n, type){
  nsz <- igraph::vcount(g)
  
  if (nsz < n){
    g <- igraph::add_vertices(g, n-nsz)
  } else if (nsz > n){
    stop(sub("graphtype", type, "The inputted structural zeros graphtype graph has more vertices than the inputted graphtype graph."))
  }
  
  return(g)
}


structural_zeros_matrix <- function(zeros.dir, zeros.bidir){
  
  adj.dir <- igraph::get.adjacency(zeros.dir, sparse=FALSE)
  adj.bidir <- igraph::get.adjacency(zeros.bidir, sparse=FALSE)
  
  n <- igraph::vcount(zeros.dir)
  
  mzeros <- 1-Get.Configuration.Matrix.p1.FW(zeros.dir, zeros.bidir)
  
  for (i in 1:n){
    for (j in 1:n){
      # directed zeros
      mzeros[i,j,1,2] <- 1 - adj.dir[i,j]
      mzeros[j,i,2,1] <- 1 - adj.dir[i,j]
      # undirected zeros
      mzeros[i,j,2,2] <- 1 - adj.bidir[i,j]
      mzeros[j,i,2,2] <- 1 - adj.bidir[i,j]
      # both
      mzeros[i,j,1,1] <- 1 - adj.dir[i,j]*adj.dir[j,i]*adj.bidir[i,j]
      mzeros[j,i,1,1] <- 1 - adj.dir[i,j]*adj.dir[j,i]*adj.bidir[i,j]
    }
  }
  
  for (i in 1:n){
    mzeros[i,i,,] <- c(0,0,0,0)
  }

  return(mzeros)
}



#######################################################################
# Get.MLE.p1.FW                                                       #
# Returns the MLE for the selected version of the p1 model            #
# in the form of an n x n x 2 x 2 matrix where                        #
# each cell i,j,k,l equals 1 if                                       #
# the dyad (i, j) in in state (k.l) where the states (k.l) are        #
# i---j: (1,1), i-->j: (1,2), i<--j: (2,1), i<->j:(2,2)        		    #                                            #
# Optional input:                                                     #
#   - zeros.dir: igraph directed graph, optional input to designate any #
#       directed edges that are structural zeros of the model           #
#   - zeros.bidir: igraph directed graph, optional input to designate   #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model                                                    #
#######################################################################
Get.MLE.p1.FW<-function(gdir, gbidir, reciprocation="edge-dependent", zeros.dir=NULL, zeros.bidir=NULL, maxiter=20, tol=0.1, print.deviation=FALSE){

  balanced_graphs <- balance_vertices(gdir, gbidir, "Warning! gbdir and gbidir have different vertex counts. Adding singleton vertices to balance counts.")
  gdir <- balanced_graphs[[1]]
  gbidir <- balanced_graphs[[1]]
  
  n <- igraph::vcount(gdir)
  
  m = Get.Configuration.Matrix.p1.FW(gdir,gbidir)
  # ensure structural zeros at diagonals
  startM =array(data=0.25, dim=c(n,n,2,2))
  for (i in 1:n){
    startM[i,i,,]=c(0,0,0,0)
  }
  
  # Ensure user-specified structural zeros are set
  if (!is.null(zeros.dir)){
    zeros.dir <- validate_structural_zeros_graph(zeros.dir, n, "directed")
  } else {
    zeros.dir <- igraph::graph.empty(n)
  }
  
  if (!is.null(zeros.bidir)){
    zeros.bidir <- validate_structural_zeros_graph(zeros.bidir, n, "undirected")
  } else {
    zeros.bidir <- igraph::graph.empty(n, directed=FALSE)
  }
  
  if (!is.null(zeros.bidir) || !is.null(zeros.dir)){
    mzeros = 1-Get.Configuration.Matrix.p1.FW(zeros.dir, zeros.bidir)
    mzeros[,,1,1] = 1
    startM[,,,] = startM * mzeros
  }
  
  
  if (reciprocation=="edge-dependent"){
    fm <- loglin(m, list(c(1,2), c(1,3,4),c(2,3,4)), fit=TRUE, start=startM, iter=maxiter, eps=tol, print=print.deviation)
  }  else if (reciprocation=="nzconst"){
    fm <- loglin(m, list(c(1,2), c(3,4),c(1,3),c(1,4),c(2,3),c(2,4)), fit=TRUE, start=startM,iter=maxiter, tol, print=print.deviation)
  } else if (reciprocation=="zero"){
    fm <- loglin(m, list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4)), fit=TRUE, start=startM,iter=maxiter, tol, print=print.deviation)
  }else{
    stop("Get.MLE.p1.FW error: reciprocation parameter option must be one of the prespecified options.")
  }
  mleMatr = fm$fit
  return (mleMatr)
}


# function for making structural zeros that are enforced by the block structure of the graph
default_beta_sbm_zeros <- function(n, blocks){
  
  k <- max(blocks)
  
  zeros_graph <- array(data=1, dim=c(n,n,k+choose(k,2),2))
  
  # ensure diagonals are structural zeros
  for (i in 1:n){
    zeros_graph[i,i, , c(1,2)] <- 0
  }
  
  # for all vertices in the same block, make sure any other edge configuration that places
  # the edges in different blocks are structural zeros
  for (i in 1:k){
    members <- which(blocks == i)
    zeros_graph[members, members, setdiff(seq(1,k+choose(k,2)), i), c(1,2)] <- 0
  }
  
  # for all edges between two distinct blocks, make sure any other edge configuration that 
  # places them between different blocks or the same block are structural zeros
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      members_i <- which(blocks == i)
      members_j <- which(blocks == j)
      offset = k*(i-1)-(i-1)*(i)/2+j-i
      zeros_graph[members_i, members_j, setdiff(seq(1,k+choose(k,2)), k+offset), c(1,2)] <- 0
      zeros_graph[members_j, members_i, setdiff(seq(1,k+choose(k,2)), k+offset), c(1,2)] <- 0
    }
  }
  
  return(zeros_graph)
}


user_defined_beta_sbm_zeros <- function(g.zeros, blocks){
  
  
  if (igraph::is.directed(g.zeros)){
    stop("The user defined structural zeros graph must be undirected.")
  }
  
  n <- igraph::vcount(g.zeros)
  k <- max(blocks)
  
  zeros_graph <- array(data=1, dim=c(n,n,k+choose(k,2),2))
  
  edgelist <- igraph::get.edgelist(g.zeros, names=FALSE)
  
  for (q in 1:dim(edgelist)[1]){
    
    u <- edgelist[q, 1]
    v <- edgelist[q, 2]
    
    if (blocks[u] == blocks[v]){
      zeros_graph[u,v,blocks[u], c(1,2)] <- 0
      zeros_graph[v,u,blocks[u], c(1,2)] <- 0
    } else{
      zeros_graph[u,v,,c(1,2)] <- 0
      zeros_graph[v,u,,c(1,2)] <- 0
    }
  }
  return(zeros_graph)
}


#######################################################################
# Get.MLE.beta.SBM
# Computes the MLE for the beta-SBM model through an iterative proportional fitting algorithm.
# Input:
#   - g, igraph, undirected object
#   - blocks, integer vector of length n (the number of vertices in g);
#       blocks[i] is the block that vertex i of g is assigned to.
# Optional Input:
#   - maxiter: integer, the maximum number of iterations to be performed in the IPS algorithm.
#   - tol: floate, the tolerance of the IPS algorithm.
#   - print.deviation, boolean, whether the IPS algorithm should print the deviation on the terminal.
#   - zeros.g: igraph undirected graph, optional input to designate     #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model  [not currently used]                              #
# Output:
#   - mleMatr, array of dimensions n x n x (k + k choose 2) x 2, for k is the
#       number of blocks;
#       represents the mle estimate of the model.
#######################################################################
Get.MLE.beta.SBM<-function(g, blocks, zeros.g=NULL, maxiter=20, tol=0.1, print.deviation=FALSE){
  
  if (igraph::is.directed(g)){
    stop("The input graph must be undirected.")
  }

  n <- vcount(g)
  
  if (length(blocks) != n){
    stop("The length of the block assignment vector must be the same as the number of vertices in the graph")
  }
  
  if (!all(seq(1, length(unique(blocks))) == sort(unique(blocks), decreasing=FALSE))){
    stop("The indices of the block IDs must be consecutive integers starting with 1.")
  }
  
  m = Get.Configuration.Matrix.beta.SBM(g, blocks)
  startM <- default_beta_sbm_zeros(n, blocks)
  
  if (!is.null(zeros.g)){
    user_defined_zeros <- user_defined_beta_sbm_zeros(zeros.g, blocks)
    startM <- startM + user_defined_zeros
  }
  
 
  fm <- loglin(m, list(c(1,4), c(2,4), c(3,4), c(1,2,3)), fit=TRUE, start=startM, iter=maxiter, eps=tol, print=print.deviation)
  mleMatr <- fm$fit
  
  return (mleMatr)
}
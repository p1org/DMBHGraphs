library(igraph)


balance_vertices <- function(g1, g2, message){
  
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

validate_structural_zeros_graph <- function(g, n, type){
  nsz <- igraph::vcount(g)
  
  if (nsz < n){
    g <- igraph::add_vertices(g, n-nsz)
  } else if (nsz > n){
    stop(sub("graphtype", type, "The inputted structural zeros graphtype graph has more vertices than the inputted graphtype graph."))
  }
  
  return(g)
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
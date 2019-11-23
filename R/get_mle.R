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
  nd = vcount(gdir)
  nb = vcount(gbidir)
  n=max(nd,nb)
  if (nd>nb){
    gbidir = add.vertices(gbidir,nd-nb)
  }
  else if (nd<nb){
    gdir = add.vertices(gdir,nb-nd)
  }
  m = Get.Configuration.Matrix.p1.FW(gdir,gbidir)
  # ensure structural zeros at diagonals
  startM =array(data=0.25, dim=c(n,n,2,2))
  for (i in 1:n){
    startM[i,i,,]=c(0,0,0,0)
  }
  
  # Ensure user-specified structural zeros are set
  if (!is.null(zeros.dir) || !is.null(zeros.bidir)){
    if (!is.null(zeros.dir)){
      nzeros.dir = vcount(zeros.dir)
      if (nzeros.dir < n)
        zeros.dir = add.vertices(zeros.dir, n-nzeros.dir)
      else if (nzeros.dir > n)
        print("The inputted structural zeros directed graph has more vertices than the inputted directed graph.")
    }else zeros.dir = graph.empty(n)
    if (!is.null(zeros.bidir)){
      nzeros.bidir = vcount(zeros.bidir)
      if (nzeros.bidir < n)
        zeros.bidir = add.vertices(zeros.bidir, n-nzeros.bidir)
      else if (nzeros.bidir > n)
        print("The inputted structural zeros undirected graph has more vertices than the inputted undirected graph.")
    }else zeros.bidir = graph.empty(n, directed=FALSE)
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
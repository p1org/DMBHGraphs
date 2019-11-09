########################################################################
# Implements the IPS algorithm for fitting the probability parameters	  #
# of the p1 model based on Holland and Leinhardt (1981) page 40			    #
# Input:																                                #
# - network: an nxn adjacency matrix for a directed graph. Used to		  #
# extract sufficient statistics: indegrees, outdegrees, and in the      #
# case of constant reciprocation, number of	reciprocated edges					#
# - reciprocation: string. if "zero" the reciprocation effect in the    #
#       model is assumed to be zero; if "nzconst" it is assumed to be a #
#       non-zero constant effect.                                       #
# - maxiter: maximal number of iterations								                #
# - tol: tolerance for declaring convergence (based on the				      #
# ell-infinity norm of the difference between observed and fitted		    #
# row and columns sums)												                          #
# Output:															                                  #
# - fit: 4x(n choose 2) vector of estimated probabilities (4 for each   #
# dyad)												                                          #
########################################################################
p1.ips.HL <- function(network, reciprocation="nzconst", maxiter = 3000, tol=1e-6){
  # outdegrees and indegrees are the row and column sums of the observed network
  outdeg = apply(network, 1, sum)
  indeg = apply(network, 2, sum)
  degree.sum = sum(indeg)

  v = length(indeg) #number of vertices
  vchoose2 = v*(v-1)/2
  if (length(outdeg)!=v){ print("error: outdeg, indegree vectors must be of same dimension")}
  M=0 			#number of bidirected edges
  for (i in 1:(v-1)){
    for (j in i:v) {
      M=M+network[i,j]*network[j,i]
    }
  }
  # initialize m_ij, a_ij, n_ij from equations (22), (23), (24) on page 40, Holland and Leinhardt 1981 p1 paper - available from JASA
  # any choices for alpha,beta,rho, theta and rhoconst should work. we pick 0.
  alpha = array(0,dim = c(1, length(outdeg)))
  beta = array(0, dim = c(1, length(outdeg)))
  rho= array(0, dim = c(1, length(outdeg)))
  theta = 0
  rhoconst=0

  k=array(0, dim=c(v,v))
  m=array(0, dim=c(v,v))
  a=array(0, dim=c(v,v))
  n=array(0, dim=c(v,v))

  for (i in 1:v){
    for(j in 1:v){
      if (i!=j){
        k[i,j] = 1+ exp(theta + alpha[i]+beta[j])+ exp(theta+alpha[j]+beta[i])+ exp(rhoconst+rho[i]+rho[j]+2*theta+alpha[i]+beta[j]+alpha[j]+beta[i])
        n[i,j] = 1/k[i,j] # probability of null edge
        m[i,j] = exp(rhoconst +rho[i]+rho[j]+2*theta + alpha[i] + alpha[j]+beta[i]+beta[j])*n[i,j]
        a[i,j] = exp(theta+alpha[i]+beta[j])*n[i,j]
      }
    }
  }

  F = array(0,dim=c(1,v))
  G = array(0,dim=c(1,v))
  H = array(0,dim=c(1,v))
  L = array(0,dim=c(1,v))
  R = array(0,dim=c(v,v))

  num.iter=0
  converge=0
  while(!converge && num.iter < maxiter){
    ############ Row step ##############
    mi = apply(m, 1, sum)
    ai = apply(a, 1, sum)
    #update F
    ### f = apply(f, 1:length(outdeg), function(x) x/(mi[i]+ai[i]))
    for (i in 1:v){
      if (outdeg[i]!=0){
        F[i]=outdeg[i]/(mi[i]+ai[i])
      }
    }
    #update K
    K= (v*(v-1) - degree.sum) /(sum(ai)+sum(n))
    for (i in 1:v){
      for(j in 1:v){
        if (i!=j){
          m[i,j] = m[i,j]*sqrt(F[i]*F[j])
          a[i,j] = a[i,j]*sqrt(F[i]*K)
          n[i,j] = n[i,j]*K
        }
      }
    }

    ########### Column step ###########
    mj = apply(m, 2, sum)
    aj = apply(a, 2, sum)
    #update G
    ### f = apply(f, 1:length(outdeg), function(x) x/(mi[i]+ai[i]))
    for (j in 1:v){
      if (indeg[j]!=0){
        G[j] = indeg[j]/(mj[j]+aj[j])
      }
    }
    #update K
    K= (v*(v-1) - degree.sum) /(sum(aj)+sum(n))
    for (i in 1:v){
      for(j in 1:v){
        if (i!=j){
          m[i,j] = m[i,j]*sqrt(G[i]*G[j])
          a[i,j] = a[i,j]*sqrt(G[j]*K)
          n[i,j] = n[i,j]*K
        }
      }
    }

    if (reciprocation=="nzconst"){
      ########## Mutual step ##########
      m.total = sum(m)
      # Update H
      if (M!=0) H=M/(m.total/2) else H=0
      # Update L
      L = (vchoose2-M) /(vchoose2-m.total/2)
      for (i in 1:v){
        for(j in 1:v){
          if (i!=j){
            m[i,j] = m[i,j]*H
            a[i,j] = a[i,j]*L
            n[i,j] = n[i,j]*L
          }
        }
      }
    }

    ########## Normalizing Step ##########
    for (i in 1:v){
      for(j in 1:v){
        if (i!=j){
          R[i,j] = m[i,j] + a[i,j] + a[j,i] + n[i,j]
          r = 1/R[i,j]
          m[i,j] = m[i,j]*r
          a[i,j] = a[i,j]*r
          n[i,j] = n[i,j]*r
        }
      }
    }
    ######### Estimate Convergence #########
    ai = apply(a, 1, sum)
    mi = apply(m, 1, sum)
    aj = apply(a, 2, sum)
    mj = apply(m, 2, sum)
    pi = ai+mi			# estimate of indegree probabilities
    pj = aj+mj 			# estimate of outdegree probabilites
    Mest = sum(m)/2     # estimate of number of bidirected edges

    #    if (reciprocation=="nzconst" && M!=0){
    pidiff=abs(pi-outdeg)/outdeg
    nanindout = which(is.nan(pidiff))
    pidiff[nanindout]=0

    pjdiff=abs(pj-indeg)/indeg
    nanindin = which(is.nan(pjdiff))
    pjdiff[nanindin]=0

    if (reciprocation=="nzconst"){
      Mdiff = abs(Mest-M)/M
      if (is.nan(Mdiff)) Mdiff=0
      maxDifference = max(pidiff,pjdiff,Mdiff)
      #      maxDifference = max(abs(pi - outdeg)/outdeg, abs(pj - indeg)/indeg, abs(Mest-M)/M)
    }else maxDifference=max(pidiff,pjdiff) #else maxDifference = max(abs(pi - outdeg)/outdeg, abs(pj - indeg)/indeg)

    if (!is.nan(maxDifference)){
      converge = (maxDifference < tol )
    }
    else {
      print("Error: NaN issue in convergence constant calculation." )
      break
    }
    num.iter = num.iter +1
  }
  if (num.iter == maxiter){
    print("Warning: calculations stopped after maximum number of iterations have been reach. There may or may not have been convergence.")
  }
  fit = numeric(4 * vchoose2)
  k=1
  for(i in 1:(v-1)){
    for(j in (i+1):v){
      fit[k:(k+3)] = c( n[i,j], a[i,j], a[j,i],m[i,j])
      k = k+4
    }
  }
  fit
  out=list()
  out[[1]]=round(fit,digits=8)
  out[[2]]=n
  out[[3]]=a
  out[[4]]=m
  out[[5]] = Mest
  return(out)
}

#######################################################################
# Get.MLE.p1.HL
# Returns the MLE in the form of an {(n choose 2) X 4} matrix, 		    #
#   with each row representing the vector 							              #
#   (pij(0,0), pij(1,0), pij(0,1), pij(1,1)) with (i,j)th row 		    #
#   appearing in lexicographic order.								                  #
# Input:
#     - gdir: directed graph
#     - gbidir: undirected graph
#     - reciprocation: string. if "zero" the reciprocation parameter in the #
#       model is assumed to be zero; if "nzconst" it is assumed to be a #
#       non-zero constant.
# Optional input:
#   - zeros.dir: igraph directed graph, optional input to designate any #
#       directed edges that are structural zeros of the model           #
#   - zeros.bidir: igraph directed graph, optional input to designate   #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model                                                    #
#######################################################################
Get.MLE.p1.HL<-function(gdir, gbidir, reciprocation="nzconst", zeros.dir=NULL, zeros.bidir=NULL, maxiter=3000, tol = 1e-03){
  if (!is.null(zeros.dir) || !is.null(zeros.bidir)){
    print("Get.MLE.p1.HL: Caution: Structural Zeros functionality is not implemented in this method.")
  }
  nd = vcount(gdir)
  nb = vcount(gbidir)
  if (nd>nb){	gbidir = add.vertices(gbidir,nd-nb)	}
  else if (nd<nb){		gdir = add.vertices(gdir,nb-nd)		}
  adjMatr = get.adjacency(gbidir)+get.adjacency(gdir)
  out = p1.ips.HL(adjMatr, reciprocation, maxiter, tol)
  mleMatr = t(matrix(out[[1]], nrow = 4))
  return (mleMatr)
}

#######################################################################
# Get.Configuration.Matrix.p1.HL                                        #
# Returns a 4x(n choose 2) matrix where each row is an indicator        #
# vector for the state of the dyad (i, j) in the order				          #
# i---j, i-->j, i<--j, i<->j										                        #
#######################################################################
Get.Configuration.Matrix.p1.HL<-function(gdir,gbidir){
  num.vertices = max(vcount(gdir), vcount(gbidir))
  nrows = num.vertices*(num.vertices-1)/2
  x = matrix(data=0, nrow = nrows , ncol=4)
  gdir.vector = as.vector(t(get.edgelist(gdir)))
  gbidir.vector = as.vector(t(get.edgelist(gbidir)))
  if(ecount(gdir)!=0){
    for(k in seq(1,2*ecount(gdir),2)){
      if(gdir.vector[k]<gdir.vector[k+1]){
        i=gdir.vector[k]
        j=gdir.vector[k+1]
        c =2
      }
      else {
        i=gdir.vector[k+1]
        j=gdir.vector[k]
        c = 3
      }
      index = num.vertices*(i-1)-(i-1)*(i)/2+j-i# index of edge (i,j) in configuration matrix
      x[index,c]=1
    }
  }
  if(ecount(gbidir)!=0){
    for(k in seq(1,2*ecount(gbidir),2)){
      if(gbidir.vector[k]<gbidir.vector[k+1]){
        i=gbidir.vector[k]
        j=gbidir.vector[k+1]
      }
      else {
        i=gbidir.vector[k+1]
        j=gbidir.vector[k]
      }
      index = num.vertices*(i-1)-(i-1)*(i)/2+j-i # index of edge (i,j) in configuration matrix
      x[index,4]=1
    }
  }
  x[,1]=matrix(1,nrow = nrows, ncol=1)-x[,2]-x[,3]-x[,4]
  return(x)
}

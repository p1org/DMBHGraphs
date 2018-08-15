###########################################################################
## Using R version 3.0.0												                        ##
## Using igraph version 0.6.5-1 -WARNING do not use an older version!! 	##
## Authors: Despina Stasi, Sonja Petrovic, Elizabeth Gross.				      ##
###########################################################################
library(igraph)
########################################################################
# Estimate.p.Value    												                              #
# Estimate the percentage of graphs in the fiber of D	has a larger          #
# goodness of fit(gof) statistic for a model specified by the user.         #
# Currently the gof statistic is the chi squared statistic measuring        #
# distance from the MLE.                                                    #
# that are further from the MLE than G,                                     #
# Input:                                                                    #
#   - gdir, igraph object directed graph, if no gbidir is given             #
#       we assume that gdir may contain reciprocated edges. if gbidir       #
#       is given we assume gdir contains only unreciprocated edges.         #
#	Optional input:                                                           #
#   - gbidir, igraph object undirected graph
#  	- model: a string signifying the appropriate model                      #
#     + "p1.HLalg.recip.nzconst": for p1 model with constant non-zero       #
#       reciprocation                                                       #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm       #
#     + "p1.HLalg.recip.zero": for p1 model with zero reciprocation effect  #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#     + "p1.recip.zero": for p1 model with zero reciprocation with      #
#       the MLE calculated using the loglin package.                    #
#     + "p1.recip.nzconst": for p1 model with constant non-zero         #
#       reciprocation with the MLE calculated using the loglin package. #
#     + "p1.recip.ed": for p1 model with edge-dependent reciprocation   #
#       with the MLE calculated using the loglin package.               #
#     + "beta.SBM"                                                      #
#   - zeros.dir: igraph directed graph, optional input to designate any #
#       directed edges that are structural zeros of the model           #
#   - zeros.bidir: igraph undirected graph, optional input to designate #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model                                                    #
#   - ignore.trivial.moves: if set to true do not use loops in p-value  #
#     estimations
#   - mleMatr: the mleMatr, in the format required by model specs       #
#		- steps.for.walk                                                    #
#		- ed.coin:  a fair ed.coin by default.                     #
#		c[1]=P(directed move); 	c[2]=P(bidirected move); c[3]=P(mixed move).#
#  	- small.moves.coin: coin to determine percentage of small moves     #
# Output:                                                               #
#   - estimated p-value between 0 and 1                                 #
########################################################################
Estimate.p.Value<-function(gdir, gbidir=graph.empty(vcount(gdir),directed=FALSE), model="p1.HLalg.recip.nzconst", zeros.dir=NULL, zeros.bidir=NULL, ignore.trivial.moves=FALSE, mleMatr=NULL, steps.for.walk=100, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), mle.maxiter = 10000, mle.tol = 0.001, beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin=0){ 
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Estimate.p.Value error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else{
      if (!is.directed(gdir)){
        gbidir = gdir
        gdir = graph.empty(vcount(gdir), directed = TRUE)
      }
    }    
  }else if (ecount(gbidir)==0){
    mixed.graph = split.Directed.Graph(gdir)
    gdir = mixed.graph[[1]]
    gbidir = mixed.graph[[2]]  
  }
    
  #Error Checking
  if(!is.simple(as.undirected(gdir,mode=c("each")))){stop("Reciprocated edges in directed graph or gdir not simple.")}
  if(!is.simple(gbidir)){stop("gbidir must be a simple graph.")}
  if(!is.directed(gdir)){stop("gdir must be a directed graph.")}
  if(is.directed(gbidir)){stop("gbidir must be an undirected graph.")}
  #Ensure graphs have same number of vertices
  nd = vcount(gdir)
  nb = vcount(gbidir)
  if (nd>nb){gbidir = add.vertices(gbidir,nd-nb)}  else if (nd<nb){gdir = add.vertices(gdir,nb-nd)}
  
  if (is.null(mleMatr)){
    print("Estimate.p.Value log: Now estimating MLE.")
    mleMatr = Get.MLE(gdir,gbidir, model, zeros.dir, zeros.bidir,  maxiter = mle.maxiter, tol = mle.tol, SBM.blocks) 
    print("Estimate.p.Value log: MLE estimate completed.")  
  }else
  {
    print("Estimate.p.Value log: Employing user-provided MLE.")
  }
  obs.gf = Get.GoF.Statistic(gdir, gbidir, model, mleMatr, SBM.blocks)
  obs.gf = round (obs.gf,digits=8)
  if (is.nan(obs.gf)){    print("Estimate.p.Value log: NaN error in calculation of GF statistic.")  }
  next.network = list(gdir,gbidir)
  count = 1
  steps.used=1
  for(i in 1: steps.for.walk){
    next.network = Get.Next.Network(next.network[[1]],next.network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)	
    if (ignore.trivial.moves==FALSE || next.network[[3]]==FALSE){
      new.gf= Get.GoF.Statistic(next.network[[1]], next.network[[2]], model, mleMatr, SBM.blocks)
      new.gf=round(new.gf, digits=8)
      # If the GoF statistic for new network is larger or equal than the GoF statistic
      # for the observed network. Note that a badly estimated MLE can give 
      # significant errors in the p-value.
      if (new.gf>=obs.gf){ count = count +1 }
      steps.used=steps.used+1
    }
  }
  return (count/steps.used)
}
################################################################################################################
################################################################################################################
# Estimate.p.Value.for.Testing
################################################################################################################
################################################################################################################
################################################################################################################
Estimate.p.Value.for.Testing<-function(gdir, gbidir=graph.empty(vcount(gdir), directed=FALSE), model="p1.HLalg.recip.nzconst", zeros.dir=NULL, zeros.bidir=NULL, ignore.trivial.moves=FALSE, mleMatr = NULL, steps.for.walk=100, ed.coin=c(1/3,1/3,1/3),nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), mle.maxiter = 10000, mle.tol = 1e-03, beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin=0){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Estimate.p.Value.for.Testing error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else if (!is.directed(gdir)){
      gbidir = gdir
      gdir = graph.empty(vcount(gdir), directed = TRUE)
    }    
  }else if (ecount(gbidir)==0){
    mixed.graph = split.Directed.Graph(gdir)
    gdir = mixed.graph[[1]]
    gbidir = mixed.graph[[2]]  
  }
  #Error Checking
  if(!is.simple(as.undirected(gdir,mode=c("each")))){ stop("Reciprocated edges in directed graph or gdir not simple.") }
  if(!is.simple(gbidir)){ stop("gbidir must be a simple graph.") }
  if(!is.directed(gdir)){  stop("gdir must be a directed graph.") }
  if(is.directed(gbidir)){		
    stop("gbidir must be an undirected graph.")
  }
  
  
  
  nd = vcount(gdir)
  nb = vcount(gbidir)
  if (nd>nb){
    gbidir = add.vertices(gbidir,nd-nb)	
  }
  else if (nd<nb){
    gdir = add.vertices(gdir,nb-nd)	
  }
  # if mleMatr was not given as argument use generate the MLE
  if (is.null(mleMatr)){
    print("Estimate.p.Value.for.Testing log: Now estimating MLE.")
    mleMatr = Get.MLE(gdir,gbidir, model, zeros.dir, zeros.bidir, maxiter = mle.maxiter, tol = mle.tol, SBM.blocks)
    print("Estimate.p.Value.for.Testing log: MLE estimate completed.")
  }else
  {
    print("Estimate.p.Value.for.Testing log: Employing user-provided MLE.")
  }
  obs.gf = Get.GoF.Statistic(gdir, gbidir, model, mleMatr, SBM.blocks)
  obs.gf = round(obs.gf, digits=8)
  if (is.nan(obs.gf)){
    print("Estimate.p.Value log: NaN error in calculation of GF statistic.")
  }
  if (obs.gf== Inf){print("Estimate.p.Value Error: Infinite GF statistic for this network.")}
  next.network = list(gdir,gbidir)
  count = 1
  int.values=c() # To estimate convergence of count/i to p-value
  gof.values=c(obs.gf) # To record the  goodness of fit statistics for all networks in walk
  steps.used=1
  for(i in 1: steps.for.walk){
    next.network = Get.Next.Network(next.network[[1]],next.network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)  
#    Plot.Mixed.Graph(next.network[[1]], next.network[[2]]) ## FOR TESTING PURPOSES 2017-03-15. 
    if (ignore.trivial.moves==FALSE || next.network[[3]]==FALSE){
      new.gf= Get.GoF.Statistic(next.network[[1]], next.network[[2]], model, mleMatr, SBM.blocks)
      new.gf=round(new.gf, digits=8)
      # If the GoF statistic for new network is larger or equal than the GoF statistic
      # for the observed network. Note that a badly estimated MLE can give 
      # significant errors in the p-value.
      if (new.gf>=obs.gf){  
        count = count +1  
      }
      steps.used=steps.used+1
      int.values<-c(int.values,count/steps.used)
      gof.values<-c(gof.values,new.gf) 
    }
  }
  return (list(count/steps.used,int.values, gof.values, mleMatr))
}
########################################################################
# split.Directed.Graph
# Auxiliary Method
# Input: D, directed graph
# Output: A list containing in this order: 
#         - unreciprocated, a directed graph containing an edge uv iff uv is
#             an unreciprocated edge in D
#         - reciprocated, an undirected graph containing an edge uv iff uv is
#             a reciprocated edge in D
########################################################################
split.Directed.Graph<-function(D){
  if (!is.directed(D)){
    print("split.Directed.Graph Warning: Caution, an undirected igraph object was used in place of a directed one. I will assume all edges are reciprocated.")
    reciprocated = D
    unreciprocated =  graph.empty(vcount(D), directed = TRUE)
  }else
  {
    reciprocated = graph.empty(vcount(D), directed = FALSE)
    #Separate reciprocated edges
    reciprocated  = graph.difference(as.undirected(D, mode=c("each")),as.undirected(D,mode=c("collapse")),byname="auto")
    unreciprocated = graph.difference(D, as.directed(reciprocated, mode=c("mutual")),byname="auto")    
  }
  return (list(unreciprocated, reciprocated))
}
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
# Get.MLE
# Estimates the MLE
# Input:
#    - model: a string signifying the appropriate model                  #
#     + "p1.HLalg.recip.nzconst": for p1 model with constant reciprocation  #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#     + "p1.HLalg.recip.zero": for p1 model with zero reciprocation #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#     + "p1.recip.zero": for p1 model with zero reciprocation with   #
#       the MLE calculated using the loglin package.                    #
#     + "p1.recip.nzconst": for p1 model with constant non-zero         #
#       reciprocation with the MLE calculated using the loglin package. #
#     + "p1.recip.ed": for p1 model with edge-dependent reciprocation   #
#       with the MLE calculated using the loglin package.               #
# Optional input: 
#   - zeros.dir: igraph directed graph, optional input to designate any #
#       directed edges that are structural zeros of the model           #
#   - zeros.bidir: igraph undirected graph, optional input to designate #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model                                                    #
# Output: 
#     - The estimated mle matrix with dimensions according to the model
#         specifications
#######################################################################
Get.MLE<-function(gdir, gbidir=graph.empty(vcount(gdir),directed=FALSE), model="p1.HLalg.recip.nzconst", zeros.dir=NULL,zeros.bidir=NULL, maxiter=3000, tol = 1e-03, SBM.blocks=NULL){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Get.MLE error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else{
      if (!is.directed(gdir)){
        gbidir = gdir
        gdir = graph.empty(vcount(gdir), directed = TRUE)
      }
    }    
  }else if (ecount(gbidir)==0){
    mixed.graph = split.Directed.Graph(gdir)
    gdir = mixed.graph[[1]]
    gbidir = mixed.graph[[2]]  
  }  

  if (!is.null(zeros.dir)){
    # Ensure Structural zeros graph has the right number of vertices
    n = max(vcount(gdir), vcount(gbidir))
    nzeros.dir =vcount(zeros.dir)
    if (!is.null(zeros.dir) && nzeros.dir!=n){
      add.vertices(zeros.dir,n-nzeros.dir)
    }    
  }

  if (!is.null(zeros.bidir)){
    # Ensure Structural zeros graph has the right number of vertices
    n = max(vcount(gdir), vcount(gbidir))
    nzeros.bidir =vcount(zeros.bidir)
    if (!is.null(zeros.bidir) && nzeros.bidir!=n){
      add.vertices(zeros.bidir,n-nzeros.bidir)
    }    
  }
  
  if (model=="p1.HLalg.recip.nzconst"){
    mleMatr = Get.MLE.p1.HL(gdir,gbidir, reciprocation="nzconst", zeros.dir, zeros.bidir, maxiter,tol)  
  }else if(model=="p1.HLalg.recip.zero"){
    mleMatr = Get.MLE.p1.HL(gdir,gbidir, reciprocation="zero", zeros.dir, zeros.bidir, maxiter,tol)
  }else if (model=="p1.recip.zero"){
    mleMatr = Get.MLE.p1.FW(gdir,gbidir, reciprocation="zero", zeros.dir, zeros.bidir, maxiter,tol)  
  }else if (model=="p1.recip.nzconst"){
    mleMatr = Get.MLE.p1.FW(gdir,gbidir, reciprocation="nzconst", zeros.dir, zeros.bidir, maxiter,tol)      
  }else if (model=="p1.recip.ed"){
    mleMatr = Get.MLE.p1.FW(gdir,gbidir, reciprocation="edge-dependent", zeros.dir, zeros.bidir, maxiter,tol)    
  }else if (model=="beta.SBM"){
    mleMatr = Get.MLE.beta.SBM(gbidir, blocks=SBM.blocks, zeros.dir, zeros.bidir, maxiter,tol)    
  }else{
    stop("Get.MLE Error: invalid model argument - model must be one of the prespecified options.")
  }
  return (mleMatr)
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
#   - zeros.dir: igraph directed graph, optional input to designate any #
#       directed edges that are structural zeros of the model   [not currently used]            #
#   - zeros.bidir: igraph directed graph, optional input to designate   #
#       any undirected(or:bidirected) edges that are structural zeros   #
#       of the model  [not currently used]                              #
# Output:
#   - mleMatr, array of dimensions n x n x (k + k choose 2) x 2, for k is the
#       number of blocks;
#       represents the mle estimate of the model.
#######################################################################
Get.MLE.beta.SBM<-function(g, blocks, zeros.dir=NULL, zeros.bidir=NULL, maxiter=20, tol=0.1, print.deviation=FALSE){
  n = vcount(g)
  k = max(blocks)
  m = Get.Configuration.Matrix.beta.SBM(g,blocks)
  
  # Ensure structural zeros get preserved
  startM =array(data=0, dim=c(n,n,k+choose(k,2),2))
  n.block = rep(0,k)
  v.block = rep(list(),k)
  for (i in 1:k){
    v.block[[i]] = which(blocks==i)
    n.block[i] = length(v.block[[i]])
    startM[v.block[[i]], v.block[[i]], i,1] = rep(.5,n.block[i]^2)
    startM[v.block[[i]], v.block[[i]], i,2] = rep(.5,n.block[i]^2)
    
    for (j in 1:n){
      startM[j,j,i,1]=0
      startM[j,j,i,2]=0
    }
  }
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      offset = k*(i-1)-(i-1)*(i)/2+j-i
      startM[v.block[[i]], v.block[[j]],  k+offset,1] = rep(.5,n.block[i]*n.block[j])
      startM[v.block[[i]], v.block[[j]],  k+offset,2] = rep(.5,n.block[i]*n.block[j])
      
      startM[v.block[[j]], v.block[[i]],  k+offset,1] = rep(.5,n.block[i]*n.block[j])
      startM[v.block[[j]], v.block[[i]],  k+offset,2] = rep(.5,n.block[i]*n.block[j])
      
    }
  }
  fm <- loglin(m, list(c(1,4), c(2,4), c(3,4), c(1,2,3)), fit=TRUE, start=startM, iter=maxiter, eps=tol, print=print.deviation)  
  mleMatr = fm$fit
  return (mleMatr)
}
#########################################################################################################
#########################################################################################################
compare.p1.MLEs<-function(mleHL,mleFW){
  num.vertices = dim(mleFW)[[1]]
  FWtoHL = array(data=0,dim=dim(mleHL))
  #  maxdiff=0
  for(i in 1:(num.vertices-1)){
    for (j in (i+1):num.vertices){
      index = num.vertices*(i-1)-(i-1)*(i)/2+j-i
      FWtoHL[index,]=as.vector(mleFW[i,j,,]) 
      #      maxdiff=max(maxdiff,mleHL[index,]-as.vector(mleFW[i,j,,]))
    }
  }
  diffMatr = abs(mleHL-FWtoHL)
  maxdiff = max(diffMatr)
  mse = sqrt(sum(diffMatr^2))/length(diffMatr)
  chisqdiff = Chi.Square.Statistic(FWtoHL,mleHL)
  return (list(maxdiff, mse, diffMatr, chisqdiff))
}
#########################################################################################################
# Get.GoF.Statistic           							                                                                  #
# Estimates goodness of fit  (GoF) statistic between current network and MLE					                        #
# Input:																                                                                      #
# - confMatr:  current network in the form of the mle 						                  	                        #
# - mleMatr: the mle or extended mle in a 4x(n choose 2) matrix	format. 	                                    #
# - model: a string signifying the appropriate model under which the mle has been calculated                  #
#     + "p1.HLalg.recip.nzconst": for p1 model with constant reciprocation                                    #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#     + "p1.HLalg.recip.zero": for p1 model with zero reciprocation #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#     + "p1.recip.zero": for p1 model with zero reciprocation effect with   #
#       the MLE calculated using the loglin package.                    #
#     + "p1.recip.nzconst": for p1 model with constant non-zero         #
#       reciprocation with the MLE calculated using the loglin package. #
#     + "p1.recip.ed": for p1 model with edge-dependent reciprocation   #
#       with the MLE calculated using the loglin package.               #
#     + "beta.SBM"
########################################################################
Get.GoF.Statistic<- function(gdir, gbidir, model="p1.HLalg.recip.nzconst", mleMatr, SBM.blocks=NULL){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Get.GoF.Statistic error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else{
      if (!is.directed(gdir)){
        gbidir = gdir
        gdir = graph.empty(vcount(gdir), directed = TRUE)
      }
    }    
  }  
  
  if (!is.igraph(gdir)||!is.igraph(gbidir)){stop("Get.GoF.Statistic Error: objects passed must be igraph objects.")}
  
  if (model=="p1.HLalg.recip.nzconst" || model=="p1.HLalg.recip.zero"){
    confMatr = Get.Configuration.Matrix.p1.HL(gdir,gbidir)    
  }else if (model=="p1.recip.nzconst" || model=="p1.recip.zero" || model=="p1.recip.ed"){
    confMatr = Get.Configuration.Matrix.p1.FW(gdir,gbidir)  
  }else if (model=="beta.SBM"){
    confMatr = Get.Configuration.Matrix.beta.SBM(gbidir,blocks=SBM.blocks)
  }
  else{
    stop("Get.GoF.Statistic Error: invalid model argument - model must be one of the prespecified options.")
  }
  return (Chi.Square.Statistic(confMatr,mleMatr))
}
########################################################################            #
# Chi.Square.Statistic      									                          	          #
# Returns chi-square statistic between matrix confMatr and the mle matrix mleMatr  #
# Input:																                                            #
# - confMatr: matrix representing network or contigency table 				              #
# - mleMatr: the mle or extended mle in the same format as confMatr                 #
########################################################################
Chi.Square.Statistic<- function(confMatr,mleMatr){
  # Check that dimensions of confMatr and mleMatr are the same
  if (length(dim(confMatr))!=length(dim(confMatr)) ||  all(dim(confMatr)!=dim(confMatr))){
    stop("Error in Chi.Square.Statistic: confMatr & mleMatr are non-conformable arrays.")
  }
  gofArr= (confMatr-mleMatr)^2./mleMatr
  indNAN = which(is.nan(gofArr))
  if (length(indNAN)>0) 
    gofArr[indNAN]=0
  gf = sum(gofArr)
  return(gf)
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
#######################################################################
# Get.Configuration.Matrix.p1.FW
# Returns an n x n x 2 x 2 matrix where each cell i,j,k,l equals 1 if  #
# the dyad (i, j) in in state (k.l) where the states (k.l) are        #
# i---j: (1,1), i-->j: (1,2), i<--j: (2,1), i<->j:(2,2)      			    #
#######################################################################
Get.Configuration.Matrix.p1.FW<-function(gdir,gbidir){
  num.vertices = max(vcount(gdir), vcount(gbidir))
  x = array(data=0, dim=c(num.vertices,num.vertices,2,2))	
  gdir.vector = as.vector(t(get.edgelist(gdir)))
  gbidir.vector = as.vector(t(get.edgelist(gbidir)))
  
  if(ecount(gdir)!=0){
    for(k in seq(1,length(gdir.vector),2)){
      i=gdir.vector[k]
      j=gdir.vector[k+1]
      x[i,j,2,1]=1
      x[j,i,1,2]=1    
    }
  }
  if(ecount(gbidir)!=0){
    for(k in seq(1,length(gbidir.vector),2)){
      i=gbidir.vector[k]
      j=gbidir.vector[k+1]
      x[i,j,2,2]=1      
      x[j,i,2,2]=1        
    }
  }
  gcompl.vector= as.vector(t(get.edgelist(graph.complementer(graph.union(as.undirected(gdir),gbidir, byname="auto")))))
  if(length(gcompl.vector)!=0){
    for(k in seq(1,length(gcompl.vector),2)){
      i=gcompl.vector[k]
      j=gcompl.vector[k+1]
      x[i,j,1,1]=1
      x[j,i,1,1]=1            
    }
  }
  return(x)
}
#######################################################################
# Get.Configuration.Matrix.beta.SBM
# Computes the configuration matrix for the beta-SBM model. Required for MLE
# computations and goodness of fit calculations.
# Input:
#   - g, igraph undirected object, the graph
#   - blocks, vector of integers, blocks[i] is the block that vertex i is assigned to
# Output:
#   - x, array n x n x (k + (k choose 2))x2 array representing the graph and its block structure.
#     Each of the first k slices [,,i,1] 1<=i<=k contains the adjacency matrix of the subgraph within each 
#     block i. The following k choose 2 slices represent the subgraphs between two blocks 1<=i<j<=n.
#     The [,,,2] slice is the complement of the [,,,1] slice.
#######################################################################
Get.Configuration.Matrix.beta.SBM<-function(g, blocks){
#  print("Updated beta.SBM method-Under Testing, though it will be eventually encompassed by the p1.SBM method.")
  n = vcount(g)
  k = max(blocks)
  x = array(data=0, dim=c(n,n,choose(k,2)+k,2))
  adj = get.adjacency(g, sparse=FALSE) # would be better to find a way to represent x as a sparse array
  
  v.block=rep(list(),k)
  
  for (i in 1:k){
    v.block[[i]] = which(blocks==i)
    x[v.block[[i]], v.block[[i]], i,1] = adj[v.block[[i]],v.block[[i]]]
    x[v.block[[i]], v.block[[i]], i,2] = 1-adj[v.block[[i]],v.block[[i]]]
  }
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      offset = k*(i-1)-(i-1)*(i)/2+j-i
      x[v.block[[i]], v.block[[j]], k+offset,1] = adj[v.block[[i]],v.block[[j]]] 
      x[v.block[[i]], v.block[[j]], k+offset,2] = 1-adj[v.block[[i]],v.block[[j]]] 
      x[v.block[[j]], v.block[[i]], k+offset,1] = adj[v.block[[j]],v.block[[i]]]  
      x[v.block[[j]], v.block[[i]], k+offset,2] = 1-adj[v.block[[j]],v.block[[i]]]  
    }
  }
  for (i in 1:n){
    x[i,i,,]=c(0,0)
  }
    return(x)
}
#######################################################################
# Write.Walk.To.File: 												#
# performs a walk and saves the consecutive networks 	    				#
# in string format to file.	Prints each network in a new line as a     #
# sequence of integers separated by commas. First two integers 	   		#
# signify first edge etc. 										    #
#######################################################################
Write.Walk.To.File<-function(gdir,gbidir, model="p1.HLalg.recip.nzconst",zeros.dir=NULL,zeros.bidir=NULL,  steps=20, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), filename = "walk.txt", beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin=0){
  write("====================", filename)
  num.cols = 2*max(ecount(gdir),ecount(gbidir)) #to pass to the write function so that all entries are in one row.
  network = list(gdir,gbidir)
  for (i in 1:steps){
    stepnum = sprintf("step %d",i)
    write(stepnum, filename,append=TRUE)
    write("====================", filename,append=TRUE)
    write("Directed Graph", filename, append=TRUE)
    write(t(get.edgelist(network[[1]])), filename, append=TRUE,ncolumns=num.cols, sep = ", ")
    write("Bidirected Graph", filename, append=TRUE)
    write(t(get.edgelist(network[[2]])), filename, append=TRUE,ncolumns=num.cols, sep = ", ")
    write("====================", filename, append=TRUE)
    network = Get.Next.Network(network[[1]],network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)
  }
}
#######################################################################
# Write.Network.To.File												    #
# Prints a network in a new line as a sequence of integers separated    #
# by commas. First two integers signify first edge etc. 			    #
#######################################################################
Write.Network.To.File<-function(gdir,gbidir, filename = "walk.txt"){
  num.cols = 2*max(ecount(gdir),ecount(gbidir)) #to pass to the write function so that all entries are in one row.
  write("Directed Graph", filename, append=TRUE)
  write(t(get.edgelist(gdir)), filename, append=TRUE,ncolumns=num.cols, sep = ", ")
  write("Bidirected Graph", filename, append=TRUE)
  write(t(get.edgelist(gbidir)), filename, append=TRUE,ncolumns=num.cols, sep = ", ")
}
#######################################################################
# Save.Walk.Plots														#
# This function performs a walk and saves the plots of the 				#
# consecutive networks to file. Currently the best way to view the 		#
# walk (in a mac) is to select all generated files and open in preview 	#
# and use the down arrow to view each one. Should possibly be replaced 	#
# by an animation function sometime in the future.						#
# ADDED OPTIONAL INPUTS: to save as single file, to plot next ntwk if move=0
#######################################################################
Save.Walk.Plots<-function(gdir,gbidir, model="p1.HLalg.recip.nzconst", zeros.dir=NULL, zeros.bidir=NULL, steps=20, ed.coin=c(1/3,1/3,1/3),nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), filename="FiberWalk", single.file=FALSE,grid=c(4,4),plot.trivial.moves=TRUE, beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin=0){
  network = list(gdir,gbidir)
  if(!single.file){
    png(sprintf("%s0.png",filename),width=800, height=600,bg="white")
    Plot.Mixed.Graph(network[[1]],network[[2]])  
    dev.off()
    for (i in 1:steps){
      network = Get.Next.Network(network[[1]],network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)
      filename = sprintf("%s%d.png",filename,i)
      png(filename,width=800, height=600,bg="white")
      Plot.Mixed.Graph(network[[1]],network[[2]])	
      dev.off()
    }     
  }else{
    pdf(filename)
    # I like to plot pictures in grid format to see more than one at a time; default = 4x4 grid
    par(mfrow = grid, mar=c(0,0,0,0)+0.1) # spacing; it goes c(bottom, left, top, right)
    Plot.Mixed.Graph(network[[1]],network[[2]])      
    for (i in 1:steps){
      network = Get.Next.Network(network[[1]],network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)
      if(network[[3]]){
        if(plot.trivial.moves){ 
          Plot.Mixed.Graph(network[[1]],network[[2]])                  
        }
      }else{
        Plot.Mixed.Graph(network[[1]],network[[2]])                        
      }
    }
    dev.off()
  }
}
#######################################################################
# Plot.Walk 															#
# Performs a walk on the fiber and plots the consecutive networks. 		#
# It does not store consecutive networks.								#
#######################################################################
Plot.Walk<-function(gdir,gbidir, model="p1.HLalg.recip.nzconst", zeros.dir=NULL, zeros.bidir=NULL, steps=20, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), ignore.trivial.moves=FALSE, beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin=0){	
  network = list(gdir,gbidir)
  # Should be replaced by an animation function sometime in the future.
  for (i in 1:steps){
    network = Get.Next.Network(network[[1]],network[[2]], model, zeros.dir, zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks, small.moves.coin)
    if (ignore.trivial.moves==FALSE || network[[3]]==FALSE)
      Plot.Mixed.Graph(network[[1]],network[[2]])	
  }
}
########################################################################
# Plot.Mixed.Graph													#
# Given gdir:   the directed part of a mixed graph,						#
#   and gbidir: the bidirected part of a mixed graph 					#							
# plot the graph.														#
# For the purposes of the p1 model view the undirected edges as 			#
# bidirected.														#
########################################################################
Plot.Mixed.Graph<- function(gdir,gbidir, arrowmd=0){
  # Prints the mixed graph, with the vertices in the shape of a circle
  gmixed = graph(t(get.edgelist(gdir)),n=max(vcount(gdir),vcount(gbidir)))
  E(gmixed)$arrow.mode = 2
  if(ecount(gbidir)!=0){
    g.became.dir = as.directed(gbidir,mode = "arbitrary")
    gmixed = add.edges(gmixed,as.vector(t(get.edgelist(g.became.dir))))
    undirected_edge_idxs = c((ecount(gdir)+1):(ecount(gdir)+ecount(gbidir)))
    E(gmixed)[undirected_edge_idxs]$arrow.mode = arrowmd
  }
  plot(gmixed,layout=layout.circle,vertex.shape="none")
}


#####################################################################################
# Get.Next.Network                                                                  #
#	Input:                                                                            #
#		- d: igraph object, directed                                                    #
#		- b: igraph object, undirected                                                  #
#	Optional input:                                                                   #
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model                                                    
#     [NOTE: zeros.dir may have both i->j and i<-j, while zeros.bidir does not have i<->j. Up to user to be clear on how they specify the zeros.]
#		- ed.coin: vector of floats, length 3; to be used for p1.ed.recip model;
#       a fair coin by default. 
#       c[1]=P(directed move); 	c[2]=P(bidirected move); c[3]=P(mixed move).
#   - nzconst.coin: vector of floats, length 2; to be used for p1 non-zero constant reciprocation
#   - SBM.blocks: vector of integers represeting the block assignment of the vertices of b, 
#       length equal to the number of vertices of b. It is mandatory input for the beta.SBM model.
#    - small.moves.coin: coin to determine percentage of small moves     
# Output:
#   - list of 3 things:
#   1) new.directed.graph,
#   2) new.bidirected.graph,
#   3) boolean flag trivial.move
# Given a mixed graph G=(d,b) returns new mixed graph G' in the p1 fiber
# F(G) with reciprocation after applying a random Graver basis element 
# The move could be 		#
#	only directed, or only bidirected, or a composite of the two.		#
# Optional input:                                                                   
#      - ed.coin is optional input; by default it's "fair": 					#
#	      + c[1]=P(directed move); 	c[2]=P(bidirected move); c[3]=P(mixed move).#
#      - model: a string signifying the appropriate model                  #
#       + "p1.HLalg.recip.nzconst": for p1 model with constant reciprocation  #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#       + "p1.HLalg.recip.zero": for p1 model with zero reciprocation #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm   #
#       + "p1.recip.zero": for p1 model with zero reciprocation effect with   #
#       the MLE calculated using the loglin package using Fienberg-Wasserman's
#       configuration matrix.                                                 #
#       + "p1.recip.nzconst": for p1 model with constant non-zero         #
#       reciprocation with the MLE calculated using the loglin package    #
#       using Fienberg-Wasserman's configuration matrix.                  #
#       + "p1.recip.ed": for p1 model with edge-dependent reciprocation   #
#       with the MLE calculated using the loglin package.                 #
#       using Fienberg-Wasserman's configuration matrix.                  #
#       + "beta.SBM": for the beta SBM model                                 #
#######################################################################
Get.Next.Network <- function(d, b, model="p1.recip.ed", zeros.dir=NULL, zeros.bidir=NULL, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(b)/(ecount(d)+ecount(b)), ecount(d)/(ecount(d)+ecount(b))), beta.SBM.coin=c(1/2), SBM.blocks=NULL, small.moves.coin = 0){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(b))
      stop("Get.Next.Network error: SBM.blocks must be same length as number of vertices in b.")
    move = Get.Move.beta.SBM(b, blocks=SBM.blocks, coin = beta.SBM.coin,small.moves.coin)
    trivial.move = move[[3]]
    #b minus bidirected.to.be.removed plus bidirected.to.be.added
    new.bidirected.graph = graph.union(graph.difference(b, move[[1]],byname="auto"), move[[2]], byname="auto")
    new.directed.graph = d
    #    print(paste("New Network"))                        #for testing
    #    print(get.edgelist(new.bidirected.graph))          #for testing
  }else{
    #p1 model
    if( !is.null(zeros.dir) && !is.igraph(zeros.dir)  ) 
      # TO DO: SHOULD PROBABLY CHECK THAT zeros.dir IS ACTUALLY DIRECTED AS WELL! 
      stop("Get.Next.Network error: zeros.dir, the optional argument for directed edge structural zeros, must be an igraph object.")
    if( !is.null(zeros.bidir) && !is.igraph(zeros.bidir)  ) 
      # TO DO: SHOULD PROBABLY CHECK THAT zeros.bidir IS ACTUALLY UNDIRECTED AS WELL! 
      stop("Get.Next.Network error: zeros.bidir, the optional argument for undirected(bidirected) edge structural zeros, must be an igraph object.")
    markov.move = Get.Move.p1(d,b,model,zeros.dir,zeros.bidir,ed.coin, nzconst.coin, small.moves.coin)
    trivial.move=FALSE
    if (!ecount(markov.move[[1]])==0 || (model=="p1.recip.ed" && !ecount(markov.move[[3]])==0)){
      if (model=="p1.HLalg.recip.nzconst" || model=="p1.HLalg.recip.zero" || model=="p1.recip.zero" || model=="p1.recip.nzconst"){
        g.as.directed = graph.union(graph.difference(graph.union(d,as.directed(b,mode="mutual"),byname="auto"),markov.move[[1]],byname="auto"),markov.move[[2]],byname="auto")
        g.as.mixed = split.Directed.Graph(g.as.directed)
        new.directed.graph = g.as.mixed[[1]]
        new.bidirected.graph = g.as.mixed[[2]]
        if (model=="p1.HLalg.recip.nzconst"|| model=="p1.recip.nzconst"){
          # Ensure number of reciprocal edges remains constant.
          if (ecount(new.bidirected.graph) != ecount(b)){
            trivial.move=TRUE
            new.directed.graph = d
            new.bidirected.graph = b
          }
        }
      }else if (model == "p1.recip.ed"){
        #d minus directed.to.be.removed plus directed.to.be.added:
        new.directed.graph = graph.union(graph.difference(d,markov.move[[1]],byname="auto"),markov.move[[2]],byname="auto")        
        
        #b minus bidirected.to.be.removed plus bidirected.to.be.added
        new.bidirected.graph = graph.union(graph.difference(b,markov.move[[3]],byname="auto"),markov.move[[4]],byname="auto")      
      }    else stop("Get.Next.Network Error: invalid model argument - model must be one of the prespecified options.")
    }else{
      #empty move, graphs unchanged
      trivial.move=TRUE
      new.directed.graph = d
      new.bidirected.graph = b
    }
  }
  return(list(new.directed.graph,new.bidirected.graph,trivial.move)) 
}

##############################################################################
# Get.Move.p1                                              					            #
#   Returns a random move, not necessarily primitive that is   		            #
#	applicable to the observed network G that is guaranteed to move		          #
#	to a network in the fiber, defined by the model, including itself.          #  
# The move could be 		                                                      #
#	only directed, or only bidirected, or a composite of the two.		            #
#                                                                             #
# Input  					                                                            #
#       - d: directed graph,   					                                      #
#       - b: bidirected graph						                                      #
# Optional input:                                                                   
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model                                                    
#     [NOTE: zeros.dir may have both i->j and i<-j, while zeros.bidir does not have i<->j. Up to user to be clear on how they specify the zeros.]
#    - ed.coin: by default it's "fair": 					                            #
#	      c[1]=P(directed move); 	c[2]=P(bidirected move); c[3]=P(mixed move).  #
#    - model: a string signifying the appropriate model                       #
#       + "p1.HLalg.recip.nzconst": for p1 model with constant reciprocation  #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm         #
#       + "p1.HLalg.recip.zero": for p1 model with zero reciprocation         #
#       and the MLE calculated with Holland-Leinhardt's IPS algorithm         #
#       + "p1.recip.zero": for p1 model with zero reciprocation effect with   #
#       the MLE calculated using the loglin package                           #
#       using Fienberg-Wasserman's configuration matrix.                      #
#       + "p1.recip.nzconst": for p1 model with constant non-zero             #
#       reciprocation with the MLE calculated using the loglin package        #
#       using Fienberg-Wasserman's configuration matrix.                      #
#       + "p1.recip.ed": for p1 model with edge-dependent reciprocation       #
#       with the MLE calculated using the loglin package                      #
#       using Fienberg-Wasserman's configuration matrix.                      #
#    - nzconst.coin: (only for p1.recip.nzconst and p1.HLalg.recip.nzconst    #
#          models) specifies the percentage of edge-dependent fiber moves that#
#          will be forced in the nzconst fiber. Set to (0,1) for zero such    #
#          moves to be used.                                                  #
#    - small.moves.coin: coin to determine percentage of small moves          #
# Output: 
#         - A list of four igraph objects representing the move
#           + directed igraph object: the directed only edges to remove
#           + directed igraph object: the directed only edges to add
#           + undirected igraph object: the reciprocated only edges to remove (only if model is p1.recip.ed)
#           + undirected igraph object: the reciprocated only edges to add (only if model is p1.recip.ed)
#######################################################################
Get.Move.p1<-function(gdir, gbidir, model="p1.recip.ed",zeros.dir=NULL,zeros.bidir=NULL ,ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), small.moves.coin=0){
  if (model=="p1.HLalg.recip.nzconst" || model=="p1.recip.nzconst" || model=="p1.HLalg.recip.zero" || model=="p1.recip.zero"){
    coin.value = runif(1)
    if (coin.value<=nzconst.coin[1]){
      mixed.move = Get.Move.p1.ed(gdir, gbidir, zeros.dir,zeros.bidir, ed.coin,small.moves.coin)  
      move=list( graph.union(mixed.move[[1]], as.directed(mixed.move[[3]],mode="mutual"),byname="auto"), graph.union(mixed.move[[2]], as.directed(mixed.move[[4]], mode="mutual"),byname="auto"))
    }else
      move = Get.Move.p1.zero.or.nzconst(gdir,gbidir,zeros.dir,zeros.bidir,small.moves.coin)     
    # Developer notes (internal): 
    # I hate R and the way it passes arguments of same type! Order determines everything for unnamed arguments. See slide 5 of https://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/functions.pdf
  } else if (model=="p1.recip.ed"){
    move = Get.Move.p1.ed(gdir,gbidir,zeros.dir,zeros.bidir, ed.coin, small.moves.coin)   
  } else{
    stop("Get.Move Error: invalid model argument - model must be one of the prespecified options.")
  }
  return (move)
} 
#################################################################################
# Get.Move.p1.ed															                                  #
# 	Given a mixed graph G=(d,b)	with d: directed graph, b: bidirected graph				#
#	returns a random move, not necessarily primitive that is 			                  #
#	applicable to the observed network G that is guaranteed to move		              #
#	to any network in the edge-dependent-reciprocation p1 fiber. The move could be 		#
#	only directed, or only bidirected, or a composite of the two.		#
#   ed.coin is optional input; by default it's "fair": 					#
#	c[1]=P(directed move); 	c[2]=P(bidirected move); c[3]=P(mixed move).#
# Input:  
#         -d: a directed graph, igraph object
#         -b: an undirected graph, igraph object
# Optional input:                                                                   
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model
#    - small.moves.coin: coin to determine percentage of small moves     #
# Output:
#         - A list of four igraph objects representing the move
#           + directed igraph object: the directed only edges to remove
#           + directed igraph object: the directed only edges to add
#           + undirected igraph object: the reciprocated only edges to remove
#           + undirected igraph object: the reciprocated only edges to add
#######################################################################
Get.Move.p1.ed <- function(d,b, zeros.dir=NULL, zeros.bidir=NULL, ed.coin=c(1/3,1/3,1/3),small.moves.coin=0){
  # if the ed.coin options do not sum up to 1 exit.
  if (! sum(ed.coin)==1) { stop("invalid ed.coin") }
  #Generate a random real number between 0 and 1.
  ed.coin.value = runif(1)
  # Now just see where ed.coin.value is in relation to the ed.coin vector (a,b,c):
  # first ed.coin option is : ed.coin \in [0.0, a]:
  if (ed.coin.value <= ed.coin[1]) { 
    dir.move = Get.Directed.Move.p1.ed(d,b,zeros.dir,small.moves.coin)     # no need to check conflict with bidirected zeros
    return(list(dir.move[[1]],dir.move[[2]], graph.empty(vcount(b),directed=FALSE), graph.empty(vcount(b),directed=FALSE)))
  }
  # second ed.coin option is: ed.coin \in (a,a+b]:
  else if (ed.coin[1]<ed.coin.value && ed.coin.value <= ed.coin[1]+ed.coin[2]) {
    bidir.move = Get.Bidirected.Move(d,b,zeros.bidir,small.moves.coin)    # no need to check conflict with directed zeros
    return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),bidir.move[[1]],bidir.move[[2]]))
  }
  # third ed.coin option is : ed.coin \in (a+b,a+b+c]:
  else if (ed.coin[2]<ed.coin.value) {
    return(Get.Mixed.Move.p1.ed(d,b,zeros.dir,zeros.bidir,small.moves.coin))     
  }
}
#################################################################################
# Get.Move.p1.zero.or.nzconst 														                                  #
# 	Given a mixed graph G=(d,b)	with d: directed graph, b: bidirected graph				#
#	returns a random move, not necessarily primitive that is 			                  #
#	applicable to the observed network G that is guaranteed to move		              #
#	to any network in the zero-reciprocation p1 fiber.
# Input:  
#         -d: a directed graph, igraph object
#         -b: an undirected graph, igraph object
# Optional input:                                                                   
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model                                                    
#    - small.moves.coin: coin to determine percentage of small moves     
# Output:
#         - A list of two igraph objects representing the move
#           + directed igraph object: the directed edges to remove from the full graph d+b
#           + directed igraph object: the directed edges to add to full graph d+b
#######################################################################
Get.Move.p1.zero.or.nzconst <- function(d,b,zeros.dir=NULL,zeros.bidir=NULL,small.moves.coin=0){
  d = graph.union(d,as.directed(b,mode="mutual"),byname="auto")
  move = Get.Directed.Move.p1.const.or.zero(d,zeros.dir,zeros.bidir,small.moves.coin)  
  return (move)
} 
##############################################################################
##############################################################################
##############################################################################
# Get.Move.beta.SBM                                                    		    #
#   Returns a random move, not necessarily primitive that is   		            #
#	applicable to the observed network G that is guaranteed to move		          #
#	to a network in the beta-SBM fiber, defined by the model, including itself. #  
#                                                                             #
# Input  					                                                            #
#       - g: undirected graph, as an undirected igraph object                 #					                                      #                  #
#       - blocks: a vector designating the block assignments of the nodes     #
#                 blocks[i]=j if vertex i is assigned to block j
#       - coin: controls 
#Optional Input:
#    - small.moves.coin: coin to determine percentage of small moves     
# Output: 
#         - A list of two undirected igraph objects representing the move
#           + undirected igraph object: the (undirected) edges to remove
#           + undirected igraph object: the (undirected) edges to add
#           + boolean flag indicating whether the move is empty
#######################################################################
Get.Move.beta.SBM<-function(g, blocks, coin=c(1/2),small.moves.coin=0){
  # Iterate through all the blocks generating a random bidirected move within
  # each block and a bipartite move between blocks.
  if(is.null(blocks)) print("Error: blocks parameter cannot be empty in Get.Move.beta.SBM.")
  n = vcount(g)
  k = max(blocks)
  
  move = list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE)
  v.block = list()
  g.block = list()
  for (i in 1:k){
    # The subgraphs within the blocks
    v.block[[i]] = which(blocks==i)
    g.block[[i]] = Get.Induced.Subgraph(g, v.block[[i]])
  }
  coin.value = runif(1)
  if (coin.value<=coin[1]){
    # This part of the code allows for moves containing the swich ab,cd<->ac,bd where block[a]=block[b]=block[c]!=block[d]
    # to be produced. These moves are both valid and necessary as such moves preserve everyone's degree in the full graph, 
    # allow degrees of indivudual vertices to change within a block and between two blocks, and do not change the number of edges
    # within block i or between block i and block j.
    # [DEVELOPER NOTE: Note that this is different from the small.moves.coin that is a possible optional input, we should change the name eventually]
    small.move.coin.value = runif(1)
    if(small.move.coin.value<.5){
      # Perform a small move guaranteed to generate a network in a different sub-fiber with different within-block vertex degrees.
      indices = sample(1:k,2)
      i = indices[1]
      j = indices[2]
      if (length(v.block[[i]])>=3 && length(v.block[[j]])>1)
          v.included = c(sample(v.block[[i]],3), sample(v.block[[j]],1))
        else if (length(v.block[[i]])>=3 && length(v.block[[j]])==1)
          v.included = c(sample(v.block[[i]],3), v.block[[j]])
        else return (move)
    }else{
      r = sample(2:k,1)
      included.blocks = sample(1:k, r)
      v.included = c()
      for (i in included.blocks){
        v.included =c(v.included, v.block[[i]])
      }
    }
    g.subgraph = Get.Induced.Subgraph(g,v.included)
    proposed.move = list(graph.empty(n),graph.empty(n,directed=FALSE))
    
    count=0
    while (ecount(proposed.move[[1]])==0 && count<50){
      proposed.move = Get.Bidirected.Move(graph.empty(vcount(g.subgraph)),g.subgraph,zeros=NULL,small.moves.coin)
      count= count+1
    }
    # Error checking making sure we are removing same total number of edges as we are adding
    if (ecount(proposed.move[[1]]) != ecount(proposed.move[[2]])){
      return(list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE))
    }
    
    graph.to.remove = graph.difference(proposed.move[[1]],proposed.move[[2]],byname="auto")
    graph.to.add = graph.difference(proposed.move[[2]],proposed.move[[1]],byname="auto")
    if (ecount(graph.to.remove)==0 && ecount(graph.to.add)==0)
      return(list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE))
    
    for (i in 1:k){
      # Check that number of edges within block i remain constant
      if ( ecount(Get.Induced.Subgraph(graph.to.remove,v.block[[i]])) != ecount(Get.Induced.Subgraph(graph.to.add,v.block[[i]])) )
        return(list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE))
      if (i<k){
        for (j in (i+1):k){
          # Check that number of edges between block i and block j remain constant
          g.full.i.j = Get.Induced.Subgraph(g,c(v.block[[1]],v.block[[2]]))
          g.between.i.j = graph.difference(g.full.i.j, graph.union(g.block[[i]], g.block[[j]],byname="auto"),byname="auto")
          num.g.between.i.j.edges.to.remove = length(which(get.edge.ids(g.between.i.j, as.vector(t(get.edgelist(graph.to.remove))))>0))
          
          num.g.between.i.j.edges.to.add = 0
          edges.to.add = get.edgelist(graph.to.add)
          for ( l in 1:ecount(graph.to.add) ){
            if ( (is.element(edges.to.add[l,1],v.block[[i]]) && is.element(edges.to.add[l,2],v.block[[j]])) || (is.element(edges.to.add[l,1],v.block[[j]]) && is.element(edges.to.add[l,2],v.block[[i]]) ) )
              num.g.between.i.j.edges.to.add = num.g.between.i.j.edges.to.add + 1
          }
          
          if (num.g.between.i.j.edges.to.add != num.g.between.i.j.edges.to.remove){
            return(list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE))            
          }
        }
      }
    }
    move[[1]] = proposed.move[[1]] #graph.to.remove
    move[[2]] = proposed.move[[2]] #graph.to.add
    if (ecount(move[[1]])>0 && ecount(graph.difference(move[[1]], move[[2]],byname="auto"))>0 && ecount(graph.difference(move[[2]], move[[1]],byname="auto"))>0)
      move[[3]]=FALSE
  }else{
    for (i in 1:k){
      # Produce a move within block i
      move.within.i = Get.Within.Blocks.Move.beta.SBM(g.block[[i]],small.moves.coin)
      move[[1]] = graph.union(move.within.i[[1]],move[[1]],byname="auto")
      move[[2]] = graph.union(move.within.i[[2]],move[[2]],byname="auto")
      if (i<k){
        for (j in (i+1):k){
          #Produce a move between block i and j
          g.full.i.j = Get.Induced.Subgraph(g,c(v.block[[1]],v.block[[2]]))
          g.between.i.j = graph.difference(g.full.i.j, graph.union(g.block[[i]], g.block[[j]],byname="auto"),byname="auto")
          move.between.i.j = Get.Between.Blocks.Move.beta.SBM(g.between.i.j,small.moves.coin) 
          move[[1]] = graph.union(move.between.i.j[[1]],move[[1]],byname="auto")
          move[[2]] = graph.union(move.between.i.j[[2]],move[[2]],byname="auto")
        }     
      }
    }
    # Error checking making sure we are removing same number of edges as we are adding
    if (ecount(move[[1]])!=ecount(move[[2]])){
      return(list(graph.empty(n, directed=FALSE),graph.empty(n, directed=FALSE), TRUE))
    }
    if (ecount(move[[1]])>0 && ecount(graph.difference(move[[1]],move[[2]],byname="auto"))>0 && ecount(graph.difference(move[[2]],move[[1]],byname="auto"))>0)
      move[[3]]=FALSE
  }
  return (move)
} 
Get.Between.Blocks.Move.beta.SBM<-function(g,small.moves.coin=0){
  d = as.directed(g, mode = c("arbitrary"))
  b = graph.empty(vcount(d), d=FALSE)
  move = Get.Directed.Move.p1.ed(d,b,zeros=NULL,small.moves.coin)
  move[[1]] = as.undirected(move[[1]])
  move[[2]] = as.undirected(move[[2]])
  return (move)
}
Get.Within.Blocks.Move.beta.SBM<-function(g,small.moves.coin=0){
  return (Get.Bidirected.Move(graph.empty(vcount(g)),g,zeros=NULL,small.moves.coin))
}
##############################################################################
# Get.Induced.Subgraph
#   Method necessary as neither induced.subgraph nor subgraph.edges gives exactly the required result
#   Alternatively one would need to use named-vertex igraph objects throughtout the code.
# Input:
#     - g: igraph object
#     - vertices: list of vertices of g
# Output:
#     - igraph object representing the subgraph of g induced by vertices and 
#       containing all vertices of g
##############################################################################
Get.Induced.Subgraph<-function(g,vertices){
  if (length(vertices)<2)
    return (graph.empty(n=length(vertices), directed=is.directed(g)))
  pairs = combn(vertices,2)
  ei = get.edge.ids(g, pairs)
  ei = ei[ei!=0]
  return (subgraph.edges(g, ei, delete.vertice=FALSE))
}
##############################################################################
##############################################################################
#######################################################################
# Get.Directed.Move.p1.const.or.zero  											#
# 	Given a mixed graph G=(d,b)										#
#		with d: directed graph, b: bidirected graph					#
#	returns a random move consisting of directed edges only				#
#	applicable to the observed network G that is guaranteed to move		#
#	to a network in the fiber, including itself. 						#
# Structural zeros of the model are also preserved because of: 
# Optional input:                                                                   
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model
#    - small.moves.coin: coin to determine percentage of small moves     
#######################################################################
Get.Directed.Move.p1.const.or.zero <- function(d,zeros.dir=NULL,zeros.bidir=NULL,small.moves.coin=0){
  # see dilemma below as to why i'm not passing zeros down further from this point.
  
  dir.piece=Get.Directed.Piece(d,zeros=NULL,small.moves.coin)   
  # 
  # DEVELOPER'S NOTES: [dilemma]
  # In summary, I do not know how to handle directed+undirected zeros in the zero-recirpocation p1 model. I have NOT resolved this yet: 
  #
  # inside this method i'll de-couple the graph d back into (d,b) to see if dir.piece violated directed zeros OR bidirected zeros. 
  # however, when i pass zeros.dir to get.directed piece, will i be screwed, i.e., will i reject moves i don't want to reject?
  # for example if I end up constructing a move that adds an edge conflicting zeros.dir, but that edge is really part of a reciprocated edge.
  # so does this ^^^ mean i just have to check -- if edge in dir.piece conflicts zeros.dir, only discard move IF 
  #   that edge in dir.piece is, in fact, NOT reciprocated in the new graph g.add. 
  # i think this ^^^ will solve the main problem i have. however: 
  # i am concerned because d now contians edges of type i->j and i<-j , when called from this method.
  # (and it's only a problem here; because when we call get.directed.piece from other methods (p1.recip.ed model), d has no reciprocated edges (they are all in b).)
  # 
  # so when we call get.directed.piece, which in turn calls bipartite.walk, <<< that method may accidentally throw away moves i actually want to keep.
  # but once we go down to get.directed.piece, we have lost track of what the model is, so i have no way of telling the method 'hey don't throw out yet'.
  #
  # the best I can think of right now is to NOT pass ANY zeros below this point, risking getting many inapplicable moves, but at least then
  # i'll be sure i'm not doing something wrong.
  # if someone has a better idea how to handle this situation - great, let me know.
  # 
  # another troubling example: if original b contained i<->j, then this d contains edges i->j and i<-j. suppose now that 
  # zeros.dir contains i->j, while zeros.bidir does not contain i<->j.
  # the moment i interpret i<->j as the union of 2 directed edges (which we do for zero recip!), we have already
  # violated zeros.dir.  so how do i deal with this? passing down  d and zeros.dir, which are already in conflict? 
  # .... 
  #
  
  if (is.null(dir.piece[[1]])){ 
    return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
  }
  else{
    ### >>>> TAG - is.applicable(dir.piece, model???)  <<<< ###
    # Ensure move is applicable
    g.add = graph(dir.piece[[2]])
    g.remove = graph(dir.piece[[1]])
    #(1) edges.to.add makes a simple graph.
    # This can happen if more than one partitions.
    if (!is.simple(g.add)) 
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
    #(2) edges.to.add does not intersect d - edges.to.remove in any direction [i.e. no conflicts created!]:
    if (!ecount(graph.intersection(graph.difference(d,g.remove,byname="auto"),g.add,byname="auto"))==0) 
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d)))) 
    #(3) Check whether g.add conflicts with structural zeros of the model:
    if (!is.null(zeros.dir)){
      # we do not want g.add to intersect the zeros directed graph, but we only want to discard this move if the conflicting edge is not reciprocated!
      # so we have to first split the g.add graph into directed and undirected parts and check each part separately.
      # (recall that the reason for this is that "d" which was input to this function is actually the union of "d" and "b" interpreted as directed.)
      mixed.graph.to.add = split.Directed.Graph(g.add)
      g.add.dir = mixed.graph.to.add[[1]]
      if (! ecount(graph.intersection(zeros.dir,g.add.dir,byname="auto")) ==0 ){
        print("Get.Directed.Move.p1.const.or.zero found a directed zeros conflict! returning empty.") # FOR TESTING
        return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
      }
    }
    if (!is.null(zeros.bidir)){
      # we do not want g.add to intersect the zeros undirected graph, but we only want to discard this move if the conflicting edge is actually reciprocated!
      # so we have to first split the g.add graph into directed and undirected parts and check each part separately.
      # (recall that the reason for this is that "d" which was input to this function is actually the union of "d" and "b" interpreted as directed.)
      mixed.graph.to.add = split.Directed.Graph(g.add)
      g.add.bidir = mixed.graph.to.add[[2]]  
      if (! ecount(graph.intersection(zeros.bidir,g.add.bidir,byname="auto")) ==0 ){
        print("Get.Directed.Move.p1.const.or.zero found an un directed zeros conflict! returning empty.") # FOR TESTING
        return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
      }
    }
    return (list(g.remove,g.add))
  }
}
#######################################################################
# Get.Directed.Move.p1.ed                                                             #
# 	Given a mixed graph G=(d,b)                                                       #
#		with d: directed graph, b: bidirected graph					                              #
#	returns a random move removing directed edges from the graph only                   #
#	applicable to the observed network G that is guaranteed to move		                  #
#	to a network in the p1 fiber under the reciprocation parameter choice specified     #
#         ... Q: which parameter choice specified? ... 
# Input:
#   - d: igraph directed object
#   - b: igraph undirected object
# Optional input:                                                                   
#   - zeros: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model
#    - small.moves.coin: coin to determine percentage of small moves     
# Output:
#         - A list of four igraph objects representing the move
#           + directed igraph object: the directed only edges to remove
#           + directed igraph object: the directed only edges to add
#           + undirected igraph object: the reciprocated only edges to remove
#           + undirected igraph object: the reciprocated only edges to add
#######################################################################
Get.Directed.Move.p1.ed <- function(d,b,zeros=NULL,small.moves.coin=0){
  #       (NOTE: the zeros optional argument is passed down from zeros.dir)
  dir.piece=Get.Directed.Piece(d,zeros,small.moves.coin)
  
  if (is.null(dir.piece[[1]])){ 
    return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
  }else{
    ### >>>> TAG - is.applicable(dir.piece, zeros, model???)  <<<< ###
    g.remove = graph(dir.piece[[1]])
    g.add = graph(dir.piece[[2]])
    # Check that edges.to.add makes a simple graph, and has no bidirected edges.
    if (!is.simple(as.undirected(g.add,mode="each")))
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
    # Check that edges.to.add does not conflict with d - edges.to.remove in any direction
    if (!ecount(graph.intersection(as.undirected(graph.difference(d,g.remove,byname="auto")),as.undirected(g.add),byname="auto"))==0) 
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))   
    # Check that edges.to.add does not conflict with bidirected part of graph:    
    if (!is.null(b)) {
      if (!ecount(graph.intersection(as.undirected(g.add),b,byname="auto"))==0){
        return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
      }
    }
    # Check whether anything conflicts with structural zeros of the model:
    if (!is.null(zeros)){
      #  if(!(is.directed(zeros))) 
      #    stop("Get.Directed.Move.p1.ed: the zeros optional argument needs to be in form of a directed graph!")
      #
      # we do not want g.add to intersect the zeros graph (both are directed a this point!)
      if (! ecount(graph.intersection(zeros,g.add,byname="auto")) ==0 ){
#        print("Get.Directed.Move.p1.ed found a zeros conflict! returning empty.") # FOR TESTING 
        return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
      }
    }
    return (list(g.remove,g.add))
  }  
}
#######################################################################
# Get.Bidirected.Move												#
# Given a mixed graph G=(d,b) d:directed, b: bidirected, returns			#
# an applicable bidirected only Markov move in the form of a list		#
# (g.remove, g.add) where g.remove is a graph containing the edges to 	#
# remove and g.add is a graph containing the edges to add.				#
# The move may be empty.
#
# Optional input:                                                                   
#   - zeros: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model
#    - small.moves.coin: coin to determine percentage of small moves     
# Output:
#         - A list of four igraph objects representing the move
#           + directed igraph object: the directed only edges to remove
#           + directed igraph object: the directed only edges to add
#           + undirected igraph object: the reciprocated only edges to remove
#           + undirected igraph object: the reciprocated only edges to add
#######################################################################
Get.Bidirected.Move <- function(d=NULL, b, zeros=NULL,small.moves.coin=0) {
  #       (NOTE: the zeros optional argument is passed down from zeros.bidir)
  if (is.null(d)){
    d = graph.empty(vcount(b))
  }
  bidir.piece = Get.Bidirected.Piece(b,zeros,small.moves.coin)
  
  if (is.null(bidir.piece[[1]])) 
    return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
  else {
    ### >>>> TAG - is.applicable(bidir.piece, model=p1???)  <<<< ###
    # Finally, check :
    g.add = graph(bidir.piece[[2]], n=vcount(b), directed = FALSE)
    g.remove = graph(bidir.piece[[1]],n=vcount(b),  directed = FALSE)
    #(1) edges.to.add makes a simple graph. This can happen if more than one partitions.
    if (!is.simple(g.add))
      return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    #(2) edges.to.add does not intersect b-edges.to.remove [i.e. no conflicts created!]:
    # and edges.to.add does not intersect d-edges.to.remove [i.e. no conflicts created!]:
    if (!ecount(graph.intersection(graph.difference(b, g.remove,byname="auto"), g.add,byname="auto")) == 0) 
      return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    #(3) neither order of edges.to.add intersects D:
    if (!is.null(d)) {
      if (!ecount(graph.intersection(as.directed(g.add), d,byname="auto")) == 0) 
        return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    }
    #(4) check whether anything conflicts the strutural zeros of the model: 
    if (!is.null(zeros)){
      #  if(is.directed(zeros)) 
      #    stop("Get.Bidirected.Move.p1.ed: the zeros optional argument needs to be in form of an undirected graph!")
      #
      # we do not want g.add to intersect the zeros graph: (both are undirected a this point!)
#     if (!ecount(graph.intersection(zeros,as.directed(g.add,mode="mutual"))) ==0 ){
      if (!ecount(graph.intersection(zeros,g.add,byname="auto")) ==0 ){
#        print("Get.Bidirected.Move.p1.ed found a zeros conflict! returning empty.") # FOR TESTING
        return(list(graph.empty(vcount(d)),graph.empty(vcount(d))))
      }
    }
    return(list(g.remove, g.add))
  }
}
#######################################################################
# Get.Mixed.Move.p1.ed													    #
# Given d: directed part of a mixed graph							    #
#   and b: bidirected part of a mixed graph,  						    #
# returns a composite move consisting of a directed piece, and a bidirected piece (either or both of the pieces can be empty.)	    #
# such that the move is applicable to the given network in the p1 fiber under the reciprocation parameter choice specified.     #
# Input:
#   - d: igraph directed object
#   - b: igraph undirected object
# Optional input:                                                                   
#   - zeros.dir: igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model           
#   - zeros.bidir: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model
#    - small.moves.coin: coin to determine percentage of small moves     #
# Output:
#         - A list of four igraph objects representing the move
#           + directed igraph object: the directed only edges to remove
#           + directed igraph object: the directed only edges to add
#           + undirected igraph object: the reciprocated only edges to remove
#           + undirected igraph object: the reciprocated only edges to add
#######################################################################
Get.Mixed.Move.p1.ed <- function(d, b,zeros.dir=NULL, zeros.bidir=NULL,small.moves.coin=0) {
  dir.piece = Get.Directed.Piece(d,zeros.dir,small.moves.coin)
  if (is.null(dir.piece[[1]])){
    g.remove.dir = graph.empty(vcount(d))
    g.add.dir = graph.empty(vcount(d))
  }else{	
    g.remove.dir = graph(dir.piece[[1]])
    g.add.dir = graph(dir.piece[[2]])
  }
  
  bidir.piece=Get.Bidirected.Piece(b,zeros.bidir,small.moves.coin)
  if (is.null(bidir.piece[[1]])){
    g.remove.bidir = graph.empty(vcount(b), directed=FALSE)   	
    g.add.bidir = graph.empty(vcount(b), directed=FALSE)
  }else{
    g.remove.bidir = graph(bidir.piece[[1]],directed=FALSE)
    g.add.bidir = graph(bidir.piece[[2]],directed=FALSE)	
  }
  
  ### >>>> TAG - is.applicable(dir.piece, bidir.piece, model="p1.ed")  <<<< ###
  # Check that the move will be applicable
  #(1) edges.to.add makes a simple graph. This can happen if more than one partitions. 
  # We also check that the directed edges to be added do not create new reciprocal edges
  if ( (!is.simple(as.undirected(g.add.dir,mode="each")))   ||  (!is.simple(g.add.bidir)) || 
         (!ecount(graph.intersection(g.add.bidir,as.undirected(g.add.dir),byname="auto"))==0) )
    return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
  #(2) edges.to.add does not intersect b-edges.to.remove [i.e. no conflicts created!]:
  if ((!ecount(graph.intersection(graph.difference(b, g.remove.bidir,byname="auto"), g.add.bidir,byname="auto")) == 0) || 
        !ecount(graph.intersection(as.undirected(graph.difference(d,g.remove.dir,byname="auto")),as.undirected(g.add.dir),byname="auto"))==0)
    ######This line causing the bug!! will fix soon (?what bug)
    return( list(graph.empty(vcount(d)), graph.empty(vcount(d)), graph.empty(vcount(b),directed=FALSE),graph.empty(vcount(b), directed=FALSE)) )
  #(3i) neither order of g.add.bidir intersects d-g.remove.dir:
  if (!is.null(d))
    if (!ecount(graph.intersection(as.directed(g.add.bidir), graph.difference(d,g.remove.dir,byname="auto"),byname="auto")) == 0)
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
  #(3ii) unordered g.add.dir does not intersect b-g.remove.bidir:
  if (!is.null(b))
    if (!ecount(graph.intersection(as.undirected(g.add.dir), graph.difference(b,g.remove.bidir,byname="auto"),byname="auto"))==0)
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
  #(4) check whether anything conflicts the strutural zeros of the model: 
  if (!is.null(zeros.dir)){
    # we do not want g.add.dir to intersect the zeros directed graph 
    if ( !ecount(graph.intersection(zeros.dir,g.add.dir,byname="auto")) ==0){
      print("Get.Mixed.Move.p1.ed found a zeros conflict in the directed part! returning empty.") # FOR TESTING
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    }
  }
  if (!is.null(zeros.bidir)){
    # we do not want g.add.bidir to intersect the zeros undirected graph 
    if ( !ecount(graph.intersection(zeros.bidir,g.add.bidir,byname="auto")) ==0){
      print("Get.Mixed.Move.p1.ed found a zeros conflict in the undirected part! returning empty.") # FOR TESTING
      return(list(graph.empty(vcount(d)),graph.empty(vcount(d)),graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    }
  }
    
  return(list(g.remove.dir, g.add.dir,g.remove.bidir, g.add.bidir)) 
}
#######################################################################
#######################################################################
Get.Directed.Piece <- function(d,zeros=NULL,small.moves.coin=0){
# Optional input:                                                                   
#   - zeros: 
#     *igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model       
#       (NOTE: this was passed as zeros.dir from above, specifically, by the get.directed... and get.mixed... functions.)
#     OR:
#     *igraph undirected graph, optional input to designate any 
#       bidirected edges that are structural zeros of the model.
#       (NOTE: this was passed as zeros.bidir from Get.Bidirected.Piece below!)
#     
  #
  # d = directed part of G.
  # pick a random subset E of edges of d and randomly shuffle it
  # (i.e., E = random sample from d of random size):
  if (ecount(d)==2) {# avoid unwanted behaviour of sample function
    random.subset.of.d=get.edges(d,1:2) 
    subset.size=2
  }
  else if (ecount(d)>2){
    small.moves.coin.value = runif(1)
    if (small.moves.coin.value<small.moves.coin){
      subset.size = sample(2:4 ,1)
    }
    else{subset.size = sample(2:ecount(d) ,1)}  #this is a random integer
    random.edge.indices = sample(1:(ecount(d)),subset.size)
    random.subset.of.d = get.edges(d,random.edge.indices)
  }
  else return(NULL)
  # randomly partition E,
  # and for every part E_i, call Bipartite.Walk(E_i)
  # and merge the edges.to.add_i from each of the partitions into a big set edges.to.add
  number.of.partitions = sample(1:(floor(subset.size/2)), 1)
  # initialize where to store the pieces of the walk:
  edges.to.add = c()
  edges.to.remove = c()
  more.edges = c()
  num.edges.left =subset.size
  s=1 #index
  while(num.edges.left>1) {
    if (num.edges.left==2) k=2 #avoid unwanted behaviour of sample function
    else k = sample(2:num.edges.left,1) #size of current part.
    if (num.edges.left-k == 1) k=k+1 #E's assumption on not leaving out that last edge hanging. 
    more.edges=Bipartite.Walk(random.subset.of.d[s:(s+k-1),],zeros) ## PASSING structural zeros down. Not sure if I love carrying an entire graph around but... what other option is there?!
    if (is.null(more.edges)) return(NULL)
    else edges.to.add = c(edges.to.add,more.edges ) 
    num.edges.left=num.edges.left-k
    s=s+k
  }
  # edges.to.remove has to be in the same format as edges.to.add, so do this:
  if ( !is.null(edges.to.add)) as.vector(t(random.subset.of.d)) -> edges.to.remove
  return(list(edges.to.remove,edges.to.add))
}
#######################################################################
#######################################################################
Get.Bidirected.Piece <- function(b,zeros=NULL, small.moves.coin=0) {
  # computes bidirected move ONLY without checks for conflicts.
  # this calls Bipartite.Walk but first checks if edges are a matching?
  # Randomly direct the entire bidirected graph and call Get.Directed.Piece
# Optional input:                                                                   
#   - zeros: igraph undirected graph, optional input to designate   
#       any undirected(or:bidirected) edges that are structural zeros   
#       of the model                                                   
#    - small.moves.coin: coin to determine percentage of small moves     #
  if (ecount(b) < 2) 
    return(NULL)
  b.directed = as.arbitrary.directed(b)	
  return(Get.Directed.Piece(b.directed,zeros,small.moves.coin))
}
#######################################################################
# Bipartite.Walk														                          #
# Given a (randomized) list of edges (edges.to.remove) return a list 	#
# of edges (edges.to.add) that complete an even closed walk by 			  #
# connecting the endpoints of successive edges.							          #
# This can be thought of as an operation on the parameter graph.      #
#
# The zeros optional argument is there for the case of structural     #
# zeros of the model. These are passed down as an igraph object whose #
# edges are the forbidden edges in the model.                         #
# There are two scenarios: 
#   - zeros: 
#     *igraph directed graph, optional input to designate any 
#       directed edges that are structural zeros of the model       
#       (NOTE: this was passed from Get.Directed.Piece.)
#     OR:
#     *igraph undirected graph, optional input to designate any 
#       bidirected edges that are structural zeros of the model.
#       (NOTE: this was passed from Get.Bidirected.Piece.)
#
# The multiplicity.bound optional argument option makes sure the      #
# moves respect the maximum number of times each edge is allowed to   #
# appear in the graph. The default value (1) ensures that only        # 
# squarefree move are produced.                                       #
# In general, multiplicity.bound is an integer denoting the maximum   #					                                  
# number of times each edge in the graph can apper.                   #
# In addition, the user can specify diffirent multiplicity bounds for #
# each edge in the graph; in this case, the optional argument is of   #
# type igraph, multiple (OR WEIGHTED?) graph.                         #
# DEVELOPER NOTES: 
# Currently, the optional argument isn't passed from anywhere, so     #
# nothing but the default can happen. AND I have not implemented the  #
# more general version when multiplicity.bound is not a numeric value.#
#######################################################################
Bipartite.Walk <- function(edges.to.remove,zeros=NULL, multiplicity.bound=1) {
  #connect head of (i+1)st edge to tail of ith edge to complete a walk:
  num.edges = nrow(edges.to.remove)
  edges.to.add = c()
  for (i in 1:(num.edges - 1)) {
    edges.to.add = c(edges.to.add, edges.to.remove[i + 1, 1], edges.to.remove[i, 2])
  }
  edges.to.add = c(edges.to.add, edges.to.remove[1, 1], edges.to.remove[num.edges,2])
  if (multiplicity.bound==1){
    # In case of the default value, we are working with simple graphs. 
    # Ensure that edges.to.add form no loops or multiple edges
    if (!is.simple(graph(edges.to.add)))   
      return(NULL)
  }else if(is.numeric(multiplicity.bound)) {
    # There is a constant integer bound on the number of times each edge may appear. 
    # Ensure that none of the edges we are attempting to add appear with multiplicity larger than allowed 
    if(any(count.multiple(graph(edges.to.add))>multiplicity.bound)) 
      return(NULL)
  }else  if(is.igraph(multiplicity.bound)) {
    # Here the user seems to want to put varying multiplicity bounds on edges (think: generalized Beta-model). 
    # So, first we need to check whether the user also passed structural zeros as optional input: 
    # - if no, then interpret the multiplicity bound of 0 as a structural zero (obviously!) 
    zeros.from.multiplicty = graph.complementer(multiplicity.bound)
    # - if yes, then multiplicty.bound better not have any zero edges, or they must match the zeros graph!; else return an error.
    print("Zeros from multiplicity.bound not yet checked/implemented.")
  }else {
    stop("Bipatite.Walk error: multiplicity.bound must be a numeric (i.e., integer) or an igraph object. 
         (The latter is not fully functional yet; it is there to allow for more general models so that the user may input 
         varying multiplicity bounds for individual edges of the graph - in form of a MULTIPLE/WEIGHTED(???) graph igraph object.")
  }
  if (!is.null(zeros)){
    # Ensure that edges.to.add isn't trying to add onto any structural zeros of the model
#    print("Bipartite.Walk is now checking for conflict with structural zeros.") # FOR TESTING 
    # if  is.directed(zeros) then check if edges.to.add(directed) intersect zeros: 
    if( is.directed(zeros)) {
      if (!ecount(graph.intersection(zeros, graph(edges.to.add),byname="auto")) == 0) 
        return(NULL)
    }else{
      # if !is.directed(zeros) then that means the graph edges.to.add should really be thought of as undirected (which is what will happen
      # once it gets passed back up the call tree), so undirect first, and then check if inersection with zeros is nonempty. 
      if (!ecount(graph.intersection(zeros, graph(edges.to.add,directed=FALSE),byname="auto")) == 0) 
        return(NULL)
    }
  }
  return(edges.to.add)
}
#######################################################################
#######################################################################
# as.arbitrary.directed													                      #
# Given an undirected graph b, return a directed graph containing an	#
# arbitrarily directed copy of each edge of b.							          #
#######################################################################
as.arbitrary.directed <- function(b) {
  # Create a directed graph out of the edges of b
  b.decr = graph(t(get.edges(b, 1:ecount(b))))
  # Pick a random integer from 0 to #edges in b
  num.edges.to.reverse = sample(0:ecount(b), 1) 
  # Direct the first num.edges.to.reverse edges in one way and the others the other way
  if (num.edges.to.reverse==0) {
    b.directed = b.decr
  }else{
    random.edge.indices = sample(1:ecount(b), num.edges.to.reverse)
    b.subset.decr = graph(t(get.edges(b, random.edge.indices))) #get.edges and get.edgelist direct edges in a different order somehow!
    el = get.edgelist(b.subset.decr, names = FALSE) ## magically swap cols to reverse direction
    b.subset.incr = graph(rbind(el[, 2], el[, 1]))
    # make the directed graph out of:
    # (reversed.edges.of.b being directed in the decreasing order) union (remaining edges in incr.order):    
    b.directed = graph.union(graph.difference(b.decr, b.subset.decr,byname="auto"), b.subset.incr,byname="auto")
  }
  return(b.directed)
}

#######################################################################
########################################################################
# Estimate.p.Value.From.GoFs
# Input: 
#		-gofs: list of goodness of fit statistics, with the first one the gof of the observed network
#       -burnsteps: the number of first entries we should ignore when estimating p-value of observed network
# Output:
#		- A list of 
#			- p-value estimate
#			- a list of p-value estimates for each step of the loop
#######################################################################
Estimate.p.Value.From.GoFs<-function(gofs, burnsteps){
  count = 0	# To estimate convergence of count/i to p-value
  p.values = c()
  for(i in (burnsteps +2):length(gofs)){
    # If the GoF statistic for new network is larger than the GoF statistic
    # for the observed network. Note that a badly estimated MLE can give 
    # significant errors in the p-value.
    if (gofs[i]>=gofs[1]){
      count = count +1
    }
    p.values = c(p.values, count/(i-(burnsteps+1)))
  }
  return (list(count/(length(gofs)-length(burnsteps)-1), p.values))
}
###############################################################################################
###############################################################################################
# Enumerate.Fiber
# Enumerates the fiber, and returns a list
#   Output: a list of
#     - the list of graphs visited, directed+bidirected parts,  
#     - a vector of counts for each graph, 
#     - the Total Variation Distance of the walk, and 
#     - a count of all empty moves made in each graph. 
###############################################################################################
Enumerate.Fiber<-function(gdir, gbidir, model="p1.HLalg.recip.nzconst", zeros.dir=NULL, zeros.bidir=NULL, numsteps=1000, ed.coin = c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), beta.SBM.coin=c(1/2), SBM.blocks=NULL,small.moves.coin=0){
  counts=c(1)
  current.network.index=1
  empty.move.counts=c(0)
  network=list(gdir,gbidir)
  
  graphsD=list()
  graphsB=list()
  graphsD[[1]]= as.numeric(t(get.edgelist(gdir)))
  graphsB[[1]]= as.numeric(t(get.edgelist(gbidir)))
  
  numGraphs=1
  
  for (i in 1:numsteps){
    # In case there are errors note that i is the number of steps 
    on.exit(return(list(graphsD, graphsB, counts, TV<-(sum(abs(as.numeric(counts)/i-1/numGraphs)))/2, empty.move.counts, i)))
    
    found.graph.flag = FALSE
    empty.move.flag=FALSE
    prev.network = network
    network = Get.Next.Network(network[[1]],network[[2]], model, zeros.dir,zeros.bidir, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks,small.moves.coin)
    #    if (ecount(graph.difference(network[[1]],prev.network[[1]]))==0 && ecount(graph.difference(network[[2]],prev.network[[2]]))==0){
    # new network is same as previous network
    #      empty.move.flag=TRUE
    #    }
    # replaced by
    if (network[[3]]==TRUE){
      # trivial move
      empty.move.flag=TRUE
      empty.move.counts[current.network.index]=empty.move.counts[current.network.index]+1
      found.graph.flag = TRUE
      counts[current.network.index]=counts[current.network.index]+1          
    }else{
      j=1
      while (!found.graph.flag && j<=numGraphs){
        if (ecount(graph.difference(network[[1]],graph(graphsD[[j]], n=vcount(gdir), directed=TRUE),byname="auto"))==0 && ecount(graph.difference(network[[2]],graph(graphsB[[j]], n=vcount(gbidir), directed=FALSE),byname="auto"))==0){
          # network is the jth graph encountered
          found.graph.flag = TRUE
          current.network.index=j
          counts[j]=counts[j]+1
        }else
          j=j+1
      }      
    } 
    if (!found.graph.flag){
      # Encountered new graph
      counts = c(counts,1)
      empty.move.counts = c(empty.move.counts,0)
      print(as.vector(counts))#testing
      
      numGraphs = numGraphs+1
      graphsD[[numGraphs]]=as.numeric(t(get.edgelist(network[[1]])))
      graphsB[[numGraphs]]=as.numeric(t(get.edgelist(network[[2]])))			
    }
  }
  
  #calculate the TV distance
  TV=(sum(abs(counts/numsteps-1/numGraphs)))/2
  return (list(graphsD, graphsB, counts, TV, empty.move.counts))
}

Write.Graphs.to.File<-function(graphs, filename){
  for (i in 1:length(graphs)){
    if (is.igraph(graphs[[i]])){		
      num.cols = 2*ecount(graphs[[i]]) #to pass to the write function so that all entries are in one row.
      write(t(get.edgelist(graphs[[i]])), filename, append=TRUE,ncolumns=num.cols, sep = ", ")	
    }
    else{
      num.cols=2*length(graphs[[i]])
      if (length(graphs[[i]])!=0){
        write(graphs[[i]], filename, append=TRUE,ncolumns=num.cols, sep = ", ")			
      }
      else { write("\n",filename, append=TRUE)}
    }
  }
}
# 
# Get.GoF.Statistics.From.File<-function(dir.graphs.filename,bidir.graphs.filename,mleMatr){
#   print("Not implemented yet.")
# }
# 
# # Enumerates the fiber for use with large fibers: writes to file the list of graps visited, directed+bidirected parts,  a vector of counts for each graph, the Total Variation Distance of the walk, and a count of all empty moves made in each graph. #NOTE: Does not currently keep track of empty moves.
# Enumerate.Fiber.to.File<-function(gdir, gbidir, numsteps=1000, ed.coin = c(1/3,1/3,1/3), filename.extension){
#   # METHOD NOT COMPLETE
#   print("Buggy code: don't know what is wrong with this method yet!")
#   counts=list(1)
#   empty.move.counts=list(0)	
#   network=list(gdir,gbidir)
#   numGraphs=1
#   num.cols.d = 2*ecount(gdir)
#   num.cols.b = 2*ecount(gbidir)
#   write(t(get.edgelist(gdir)), paste(filename.extension, "dir.graphs.txt", sep="."), append=FALSE,ncolumns=num.cols.d, sep = ", ")	
#   write(t(get.edgelist(gbidir)), paste(filename.extension, "bidir.graphs.txt", sep="."), append=FALSE,ncolumns=num.cols.b, sep = ", ")	
#   
#   for (i in 1:numsteps){
#     #		on.exit(print(paste("Number of steps: ", numsteps, " ------- Number of Graphs discovered: ", numGraphs, "\n total variation distance: ",(sum(abs(counts/numsteps-1/numGraphs)))/2, "\n counts: \n", counts, "\n empty move counts: \n", empty.move.counts)))
#     flag = FALSE
#     empty.move.flag=FALSE
#     prev.network = network
#     network = Get.Next.Network(network[[1]],network[[2]],ed.coin)
#     if (ecount(graph.difference(network[[1]],prev.network[[1]]))==0 && ecount(graph.difference(network[[2]],prev.network[[2]]))==0){
#       empty.move.flag=TRUE 			# new network is same as previous network
#     }		
#     # OPEN CONNECTION  
#     conD = file(paste(filename.extension, "dir.graphs.txt", sep="."), open = "r")
#     conB = file(paste(filename.extension, "bidir.graphs.txt", sep="."), open ="r")
#     graph.index = 0
#     # (WHILE FILE HAS MORE GRAPHS TO READ: READ GRAPH, COMPARE IT TO CURRENT. REST AS BEFORE)
#     while ( (length(str.dir.graph <- readLines(conD, n = 1, warn = FALSE)) > 0) && 
#               (length(str.bidir.graph <- readLines(conB, n = 1, warn = FALSE)) > 0) && flag==FALSE) {
#       #		dir.graphs = readLines(con = paste(filename.extension, "dir.graphs.txt", n = 50)		
#       #		bidir.graphs = readLines(con = paste(filename.extension, "bidir.graphs.txt", n = 50)
#       graph.index = graph.index+1
#       
#       dir.what = unlist(strsplit(str.dir.graph,split=','))
#       bidir.what = unlist(strsplit(str.bidir.graph,split=','))
#       dir.graph = as.numeric(dir.what)
#       bidir.graph = as.numeric(bidir.what)
#       d = graph(  dir.graph, n = vcount(gdir), directed = TRUE)
#       b = graph(bidir.graph, n = vcount(gbidir), directed = FALSE)
#       
#       if (ecount(graph.difference(network[[1]],d)) == 0 && ecount(graph.difference(network[[2]],b)) == 0){
#         # Current network was visited before
#         counts[[graph.index]]=counts[[graph.index]]+1
#         flag = TRUE
#         if (empty.move.flag==TRUE){
#           empty.move.counts[[graph.index]]=empty.move.counts[[graph.index]]+1
#         }
#       } 
#     }
#     # CLOSE CONNECTION  
#     close(conD)
#     close(conB)
#     if (!flag){
#       # Encountered new graph
#       counts = append(counts, list(1))
#       empty.move.counts = append(empty.move.counts,list(0))
#       numGraphs = numGraphs+1
#       # Write new graph to files 
#       ### CAUTION: writing to the file line by line can be very time consuming. 
#       ### Rewrite this to only write to file every so often (every 1000 graphs?)
#       write(t(get.edgelist(network[[1]])), paste(filename.extension, "dir.graphs.txt", sep="."), append=TRUE,ncolumns=num.cols.d, sep = ", ")	
#       write(t(get.edgelist(network[[2]])), paste(filename.extension, "bidir.graphs.txt", sep="."), append=TRUE,ncolumns=num.cols.b, sep = ", ")	
#     }	
#     
#     # Logging information. Time consuming, but avoids losing all info if the program is interrupted early
#     
#     # Write counts to file
#     write(counts, paste(filename.extension, "distinct.graph.counts.txt", sep="."), append=FALSE, ncolumns=length(counts), sep = ", ")	
#     # Write empty move counts to file
#     write(empty.move.counts, paste(filename.extension, "empty.move.counts.txt", sep="."), append=FALSE, ncolumns=length(counts), sep = ", ")	
#     
#   }
#   # calculate the TV distance
#   TV=(sum(abs(as.numeric(counts)/numsteps-1/numGraphs)))/2
#   write(TV, paste(filename.extension, "tv.distance.txt", sep="."), append=FALSE, ncolumns=length(counts), sep = ", ")	
#   return (numGraphs)
# }

############################################################################
#Random Draw Methods
############################################################################
Draw.Random.Graph.From.Model.beta<-function(betas){
  # Notes: Identifiability requires additional linear constraints on parameters, e.g. sum(betas)=0. 
  # This method leaves it up to the user to specify the parameters, and does not assume the additional constraints.
  blocks = rep(1,n)
  block.alphas = array(c(0.0), dim=c(1,1))
  return (Draw.Random.Graph.From.Model.beta.SBM(betas, block.alphas, blocks))
}

Draw.Random.Graph.From.Model.SBM<-function(n, block.alphas, blocks){
  # Notes: Identifiability requires additional linear constraints on parameters, e.g. sum(block.alphas)=0. 
  # This method leaves it up to the user to specify the parameters, and does not assume the additional constraints.
  betas = rep(0.0, n)
  return (Draw.Random.Graph.From.Model.beta.SBM(betas, block.alphas, blocks))
}

Draw.Random.Graph.From.Model.beta.SBM<-function(betas, block.alphas, blocks){
  # Notes: Identifiability requires additional linear constraints on parameters, e.g. sum(betas)=0. 
  # This method leaves it up to the user to specify the parameters, and does not assume the additional constraints.
  n=length(betas)
  k = dim(block.alphas)[1]
  g = graph.empty(n, directed = FALSE)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      exp.sum.betas=exp(betas[i]+betas[j] + block.alphas[blocks[i],blocks[j]])
      prob.edge=(exp.sum.betas)/(1+ exp.sum.betas)
      coin = sample(c(TRUE,FALSE), size=1,prob=c(prob.edge,1-prob.edge))
      if (coin)
        g = add.edges(g,c(i,j))
    }
  }
  return(g)
}



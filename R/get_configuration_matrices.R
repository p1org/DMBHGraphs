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

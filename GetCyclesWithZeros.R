################################################################################################
# This code loads all the necessary functions to generate one random applicable Markov move 
# for the independence model on a 2-dimensional contingency table with structural zeros. 
#
# It does so by taking input consisting of 1) an underlying graph of the problem (i.e., the 
# parameter hypergraph of the log-linear model), and 2) the current table or graph as a list of 
# edges. It returns one move, or an empty move if it gets unlucky in that one step. 
# 
# Load this file first, and then see ExamplesToGetCyclesWithZeros.R for how to run the code.
# 
# Author:   Sonja Petrovic <Sonja.Petrovic@iit.edu>
# For working with: Tobias Windisch <windisch@ovgu.de>
# Date: January 2016.  Ongoing work May 2016 - reworking so it fits within grand scheme of DMBHGraph!
################################################################################################


###### prerequisites 
library("igraph")
# source('~/Dropbox/Code-DynamicMarkovForParameterGraphs/p1walk-refactoring.R')


#######################################################################
# Get.Structural.Zeros
# Input:  parameter hypergraph
# Output: list of edges that are not allowed, 
#         that is, that corrspond to structural zero cells in the table
Get.Structural.Zeros<-function(parameter.graph){
  zeros.graph=graph.complementer(graph(parameter.graph.edges,d=FALSE))
  #num.vtces = length(V(graph(parameter.graph.edges)))
  #zeros.graph=graph.complementer(graph(parameter.graph.edges,n=num.vtces,d=FALSE))
  return(c(t(get.edgelist(zeros.graph))))
}
#######################################################################


#######################################################################
# Get.Structural.Zeros.Graph
# Input:  parameter.graph = parameter hypergraph, the underlying graph of the problem
# Output: the graph whose edges are the edges that are not allowed in the model, 
#         that is, that corrspond to structural zero cells in the table
Get.Structural.Zeros.Graph<-function(parameter.graph){
  zeros.graph=graph.complementer(graph(parameter.graph.edges,d=FALSE))
  return(zeros.graph)
}
#######################################################################



#######################################################################
# Get.Bidirected.Move.Zeros
# Input:  
#         d = directed graph (EMPTY in this set of examples, but remains as a general placeholder),
#         b = undirected graph, representing the current graph / table, 
#         zeros = a list of fobidden edges (in vector format)
# Output: 
#         A list of two vectors: edges to remove and edges to add to the current graph.
# Notes: 
#         The move this function outputs may be empty. 
Get.Bidirected.Move.Zeros <- function(d, b, zeros) {
  bidir.piece = Get.Bidirected.Piece(b)
  if (is.null(bidir.piece[[1]])) 
    return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
  else {
    g.add = graph(bidir.piece[[2]],vcount(b), directed = FALSE)
    g.remove = graph(bidir.piece[[1]], vcount(b),directed = FALSE)
    # I ONLY NEED TO CHECK IF I'M ATTEMPTING TO ADD TO STRUCTURAL ZEROS,  WHICH IS NOT ALLOWED:
    # edges.to.add does not intersect zeros [i.e. no edges placed into structural zerso!]:
    if (!ecount(graph.intersection(graph(zeros,directed=FALSE), g.add)) == 0) 
      return(list(graph.empty(vcount(b), directed=FALSE),graph.empty(vcount(b), directed=FALSE)))
    }
    return(list(g.remove, g.add))
  }
#######################################################################


#######################################################################
# Get.Next.Network.Zeros  
# Input:  
#         d = directed graph (EMPTY in this set of examples, but remains as a general placeholder),
#         b = undirected graph, representing the current graph / table, 
#         zeros = a list of fobidden edges (in vector format)
# Output: 
#         The network obtained by applying a random move obtained by Get.Bidirected.Move.Zeros
#         true/false flag to indicated if a move was empty (no cycle produced on this attempt)
# Notes:
#         We may stay at the same place. 
Get.Next.Network.Zeros <- function(d,b,zeros){        #},coin=c(0,1,0){
  empty.move=TRUE
  bidir.move = Get.Bidirected.Move.Zeros(d,b,zeros)
  markov.move = list(graph.empty(vcount(d)),graph.empty(vcount(d)),bidir.move[[1]],bidir.move[[2]])
  if (!ecount(markov.move[[3]])==0){ 
    print("edges to remove:")
    str(markov.move[[3]])
    print("edges to add:")
    str(markov.move[[4]])
  }
  if (!ecount(markov.move[[1]])==0 || !ecount(markov.move[[3]])==0){
    empty.move=FALSE
    new.directed.graph = d
    #    new.bidirected.graph = graph.union(graph.difference(b,markov.move[[3]]),markov.move[[4]])
    # oops have to worry about graph.union taking only simple graphs - it messes me up slightly. here is a quick fix:
    new.bidirected.graph = graph.adjacency(get.adjacency(graph.difference(b,markov.move[[3]]))+get.adjacency(markov.move[[4]]),mode="undirected")
    # I CAN DO WEIGHTED GRAPHS! see this: 
    # http://stackoverflow.com/questions/31417071/graph-union-summing-edge-weights-attributes-igraph-r
  } 
  else{
    #empty move, graphs unchanged
    new.directed.graph = d
    new.bidirected.graph = b
  }
  return(list(new.directed.graph,new.bidirected.graph,empty.move))		 
}
#######################################################################







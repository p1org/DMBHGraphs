##########################################################################################
# This file loads the necessary functions from GetCyclesWithZeros.R, 
# as well as background code p1walk.R, 
# and has an example on how to run the code in general. 
#
# Author:   Sonja Petrovic <Sonja.Petrovic@iit.edu>
# For: Tobias Windisch <windisch@ovgu.de>
# Date: January 2016. 
##########################################################################################


##########################################################################################
#   PREREQUISITES: 
##########################################################################################
setwd("~/Dropbox/DynamicMarkov-p1/R code") #change this of course
# Go and download the following file first: p1walk.R from this location: 
# http://math.iit.edu/~spetrov1/DynamicP1supplement/p1walk.R 
source('p1walk.R') # See comment on line 19 of GetCyclesWithZeros.R # 
setwd("~/Dropbox/Tobias") #change this of course
source('GetCyclesWithZeros.R')
source('p1AndMoreWalk.R') 


options(error=traceback)

# I like to plot pictures in 2x2 format to see more than one at a time: 
par(mfrow = c(2,2), mar=c(0,0,0,0)+0.1)#1, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right)
##########################################################################################




##########################################################################################
# Example 1: Tobias' toy example 
# This section sets up the graph and sets up the zeros
##########################################################################################
#
# ------ The observed graph ------
#
# there are no directed edges:
D.tob = graph.empty(8)
# the graph is a 4x4 table from indep. model w/ structural zeros; so it's a subraph of a
# undirected bipartite graph on 4+4 vertices - lets call them 1..4 and 5..8: 
#{x1,y1},{x1,y1},{x1,y1},
#{x2,y4},
#{x3,y2}, {x3,y2}, {x3,y2}, {x3,y2}, {x3,y2}, {x3,y2}, {x3,y2},
#{x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}, {x4,y3}
B.tob = graph(c(1,5, 1,5, 1,5, 2,8, 3,6, 3,6, 3,6, 3,6, 3,6, 3,6, 3,6, 4,7, 4,7, 4,7, 4,7, 4,7, 4,7, 4,7, 4,7, 4,7), d=FALSE)
Plot.Mixed.Graph(D.tob, B.tob)
# If i want to save the graph: 
#write.table(Get.Configuration.Matrix(D.tob, B.tob), file="~/Dropbox/Tobias/the-graph-config-mtx.txt", col.names=F, row.names=F)
#
# ------ The parameter graph ------
#
#the underlying graph of the problem is
#{x1,y1},{x1,y3},
#{x2,y1},{x2,y3},{x2,y4},
#{x3,y1},{x3,y2},{x3,y4},
#{x4,y2},{x4,y3}
# MISSING edges are structural zeros, but also note that the underlying graph must be bipartite. 
#
# Here is how to store this: 
# Record the vector of edges of the underlying graph of the problem (i.e. list all 'legal' table cells; missing edges are structural zeros of the model.)
# To not mess with the old code much, input the vertices of the graph in numerical order: so x1..x4 are 1..4 and y1..y4 are 5..8.
parameter.graph.edges=c(1,5, 1,7, 2,5, 2,7, 2,8, 3,5, 3,6, 3,8, 4,6, 4,7)
#num.vtces=8
#
# ------ Getting the zeros stored ------
# 
# To store them to use in the code below:
zeros = Get.Structural.Zeros(parameter.graph.edges,num.vtces)
#
# and IF you would like to see them plotted: 
zeros.graph=Get.Structural.Zeros.Graph(parameter.graph.edges,num.vtces)
print("here is the set of forbidden edges")
str(zeros.graph) 
plot(zeros.graph, layout=layout.circle)
##########################################################################################


##########################################################################################
# Running the example: 
# This section uses the graphs set up above to produce moves 
##########################################################################################
# 
# how many times would you like to try to make a move? 1 step often fails to construct a
# nonzero move, so just do more
steps.for.walk=150
# initialize: 
next.network = list(D.tob,B.tob)
# start my plots from scratch:
par(mfrow = c(2,2), mar=c(0,0,0,0)+0.1)#1, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right)
Plot.Mixed.Graph(next.network[[1]],next.network[[2]]) 
for(i in 1: steps.for.walk){
  next.network = Get.Next.Network.Zeros(next.network[[1]],next.network[[2]],zeros); 
  if (!next.network[[3]]) { 
    print("I found an applicable move! Plotting. Moving on.")
    print("------------------------------------------------")
    Plot.Mixed.Graph(next.network[[1]],next.network[[2]]) 
  }
}
##########################################################################################

# SAVING THE PLOTS OF A WALK IN THE FILE: 
Save.Walk.Plots(gdir,gbidir,single.file=TRUE,grid=c(3,3),plot.trivial.moves=FALSE)



##########################################################################################
# Other notes:
# 
# If it is helpful, I can also use Enumerate.Fiber to save these into a file - that way, 
# you can have all the graphs visited in the current walk stored somewhere. 
# 
##########################################################################################


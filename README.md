This repository is working off of p1walk.R (see http://link.springer.com/article/10.1007%2Fs10463-016-0560-2).

# Supplemental code for Goodness of fit for log-linear ERGMs

# Prerequisites 

```{r echo=TRUE, include=FALSE}
library("igraph")
```
```
library("igraph")
```

Load the necessary functions we wrote for testing GoF:
```{r}
# set your working directory
# setwd("~/GoF/Code/")
# in this file I am using "myFolderName" for storing the simulation files. 

# get the required functions: 
# download the following 3 files from the submission supplement:  
source('basicWalk.R')
source('histogramsForWalks.R')
source('Test.Model.Fit.R')
```

# Test Model Fit Examples

## Small example for $p_1$ model without reciprocation effects

Here is one small graph on 10 nodes: 
```{r}
D.HL2 = graph(c(1,2,1,7,2,1,2,3,2,5,3,2,3,7,3,9,4,1,4,3,4,7,5,1,5,2,5,3,5,4,5,6,5,7,5,8,6,1,6,2,6,3,6,4,6,5,7,2,7,3,8,1,8,5,8,6,9,2,10,2,10,4))
plot(D.HL2,layout=layout.circle,vertex.shape="none")
```

We will run the goodness of fit test of the zero-reciprocation $p_1$ model. First we do it without including any trivial moves, that is, we only iclude the steps in the Markov chain that do not produce moves that are not applicable. This does not change the statistics, but *does increase effective sample size.*
```{r}
iters = 3
steps = 2000
mle = Get.MLE(D.HL2, model="p1.recip.zero", tol=0.01, maxiter=10000)
HL2.no.trivial.moves = Test.Model.Fit(gdir=D.HL2, foldername=myFolderName, numSteps=steps, iterations=iters, model="p1.recip.nzconst", ignore.trivial.moves=TRUE, mleMatr=mle, testname="HL2")
```

We can repeat the same test but keep the number of samples to 2000, regardless of rejected proposed steps the chain might encounter: 
```{r}
HL2.with.trivial.moves = Test.Model.Fit(gdir=D.HL2, foldername=myFolderName, numSteps=steps, iterations=iters, model="p1.recip.nzconst", mleMatr=mle, testname="HL2")
```

## Small example for the beta-SBM model 

This is the exponential family version of the degree-corrected SBM, where the block membership is *known* a priori. 

```{r}
########################################################################################################################################################################
########################################################################################################################################################################
# beta SBM model Example
g = graph(c(1,2,2,3,3,4,4,1, 5,6,7,5,6,8, 4,5,3,6,2,7,1,8), d=FALSE)
plot(g, layout= layout.circle)
my.blocks = c(1, 1, 1, 1, 2, 2, 2, 2)

iters = 3
steps = 2000 # I am setting steps to a lower value for illustration purposes. The knitted file used steps=20000. 
mle.b.SBM = Get.MLE(g, model="beta.SBM", tol=0.01, maxiter=10000, SBM.blocks = my.blocks)
test.b.SBM.with.trivial.moves = Test.Model.Fit(g, foldername=myFolderName, numSteps=steps, iterations=iters, model="beta.SBM", mleMatr=mle.b.SBM, testname="betaSBM",SBM.blocks = my.blocks)
test.b.SBM.no.trivial.moves = Test.Model.Fit(g, foldername=myFolderName, numSteps=steps, iterations=iters, model="beta.SBM", ignore.trivial.moves=TRUE, mleMatr=mle.b.SBM, testname="betaSBM", SBM.blocks = my.blocks)
```

In addition the user may wish to save all of the plots from the random walk on the model fiber, as well as print all graphs visited. We do not provide output for this, for obvious space considerations.
```
Save.Walk.Plots(graph.empty(vcount(g)), g, model = "beta.SBM", steps=500, SBM.blocks=my.blocks, plot.trivial.moves=FALSE,single.file=TRUE, filename="/Users/sonjapetrovic/temp/betaSBM.simple.walk.pdf" )
enum = Enumerate.Fiber(graph.empty(vcount(g)), g, model = "beta.SBM", numsteps=500, SBM.blocks=my.blocks)
``` 


# Examples in the paper 

Each of the examples below require the three R files that contain the implementation of the test. The files below will read the data sets, and set up the tests, and then execute them. 

## Neuronal network from Section 4.2

* Test of model fit for the dyad-specific $p_1$ model on the neuronal chemical (directed) network: `NeuronalDirected.P1dyaddependent.Parallel.ChisqDiagnostics.R`. 

* Test of model fit of the $\beta$-SBM model to the neuronal mixed network: 
`NeuronalBoth.betasbm.Serial.ChisqDiagnostics.R`.  

* Test of model fit for the $p_1$ model with dyad-specific reciprocation effect to the neuronal mixed network consisting of the chemical (undirected) and gap junction (directed) graph: 
`NeuronalDirectedAndUndirected.P1dyaddependent.Parallel.ChisqDiagnostics.R`. 

The above R files require the following: `NeuronalNetworkBothWODirectionAndBlocksBasedonType.R`, `NeuronalNetworkDirected.R`, and `NeuronalNetworkDirectedandUndirected.R` which are setup to read the data files in proper (network) format from .csv raw data. 

## Protein-protein interaction network from Section 4.3

* Testing the $p_1$ model with dyad-specific reciprocation on the protein-protein interaction network with a large number of structural zero edges:   `ProteinProtein.P1dyaddependent.Zeros.Parallel.ChisqDiagnostics.R`.

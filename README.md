This repository contains supplementary material for the paper ``Goodness-of-fit for log-linear network models: Dynamic Markov bases using hypergraphs'' by Elizabeth Gross, Sonja Petrovic, and Despina Stasi (http://link.springer.com/article/10.1007%2Fs10463-016-0560-2).
In particular, it contains the R code necessary to carry out the p-value computations discussed in the paper. 

The code from that paper can only handle the p1 model. 
In this version, we extend it by allowing structural zeros to appear, lift the 0/1 constraint on the matrix, but we do this only for the independence model for now. [nxn table; calls undirected walks from p1 on a bipartite undirected graph.] 

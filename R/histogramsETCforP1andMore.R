########################################################################
# Mcmc.Diagnostics
# Given some summary of the output of an MCMC run, plot the usual
# MCMC diagnostics
#
# Input:
#   - mcmcStatsList, a list of statistics for which we will run autocorrelation and trace plots.
#     (This list is turned into an MCMC object inside of the function.)
# Output:
#   - a file with trace and autocorr. plots for the given mcmc statsitics.
#
Mcmc.Diagnostics <- function(mcmcStatsList, statsNamesList, dataname="Set title w/ option 'dataname'",filename='GoF testing plots'){  # }, grid=c(2,2)){
  require("coda"); #for mcmc diagnostics
  # ========================= #
  #  # CODE FROM VISHESH k-core ergm:
  #  mcmcStats = cbind(sampShellDist,sampDegreeDist,
  #                    sampBetweeness,sampTri,
  #                    sampEdge,sampCentrality);
  #  colnames(mcmcStats) = c(paste('shell',seq(from=0,to=n-1),sep=""),
  #                          paste('degree',seq(from=0,to=n-1),sep=""),
  #                          paste('betweenness',seq(from=1,to=n),sep=""),
  #                          "triangles",
  #                          'edges',
  #                          'centrality');
  #  mcmcStats = as.mcmc(mcmcStats);
  # ========================= #
  mcmcStats=cbind(mcmcStatsList)
  colnames(mcmcStats)=statsNamesList
  mcmcStats=as.mcmc(mcmcStats)

  save.image(paste(filename,"McmcDiag.Rdata",sep=""));
  #load(paste(name,".Rdata",sep=""));
  pdf(paste(filename,"MCMCdiagnostics.pdf"));
  #summary(mcmcStats);
  plot(mcmcStats);
  autocorr.plot(mcmcStats);
  # ========================= #
  dev.off();
}

########################################################################
# GoF.Testing.Plots
# Given outputs of a single GoF simulation
# (such as those obtained from a run of Estimate.p.Value.For.Testing),
# plot histogram of GoF statisitcs and p-values for the random walk.
#
# Input:
#   - gof.values, a list of values of the GoF statistic from a random walk, whose 1st entry is the OBSERVED ONE
#   - p.value.estimates, a list of p-value esitmates from a random walk
#	Optional input:
#   - filesave, a Boolean indicating whether to save plots to pdf file
#   - filename,
#   - burn, integer number of burn-in steps; 0 default
#   - grid, vector c(i,j) indicating plots to be made on an ixj grid
#   - .....[list of other things one may want to plot?] - UNFINISHED
# Output:
#   - plots (or saved file with plots)
# Typical usage of this function:
#   fiber.walk=Estimate.p.Value.for.Testing(D,B,steps.for.walk=10)
#   gof.values = fiber.walk[[3]]
#   p.value.estimates = fiber.walk[[2]]
#   GoF.Testing.Plots(gof.values,p.value.estimates,filesave=FALSE)
########################################################################
plot.dmbh <- function(x, dataname="Set title w/ option 'dataname'", burn=0, filesave=FALSE,filename='GoF testing plots', grid=c(2,2)){


  gof.values <- x$gof.values
  p.value.estimates <- x$p.values

#  print("This is still a little shitty b/c I don't check if input is of correct format..e.g., the two lists should be of same length if they come from the same simulation. For typical usage of the function, see function header @code. ")

  # the following only makes sense if dataname has no special characters!! but i would have really preferred to set it this way:
  #       if(filesave){pdf(paste('GoF testing plots for',dataname))}
  # otherwise i resort to 1 filename each time:
  if(filesave){
    pdf(filename) # open file connection
  }
  # plot in grid format; default = 2x2 grid
  par(mfrow = grid, mar=c(2,1,0,0)+3) # spacing; it goes c(bottom, left, top, right)

  # 1) plot p-value estimates:
  plot(p.value.estimates, main=dataname,sub=paste(length(p.value.estimates),"steps"),  xlab="Length of Walk", ylab="p-values", ylim=c(0,1), pch='.',lwd=0.25 )

  # 2) plot the sampling distribution of the GoF statistic:
  hist(gof.values,main=dataname, xlab="Goodness of Fit Statistic", ylab="frequency")
  abline(v=gof.values[1], col="red")

  # 3) MAYBE someone has burn-in:
  if(burn>0){
    p.estimates = Estimate.p.Value.From.GoFs(gof.values, burn)
    plot(p.estimates[[2]],  main=dataname ,sub=paste(length(p.estimates[[2]]),"steps"),  xlab=paste("Length of Walk (after", burn,"burn-in steps)"), ylab="p-values", ylim=c(0,1), pch='.',lwd=0.25)
    hist(gof.values[burn+2:length(gof.values)],main=dataname, xlab="Goodness of Fit Statistic", ylab="frequency")
    abline(v=gof.values[1], col="red")
  }

  if(filesave){
  #  print(paste("Plots are saved to",filename,"in the working directory (check with getwd())."))
    dev.off()  # close file connection.
  }
  # turn off grid printing:
  par(mfrow = c(1,1))
}

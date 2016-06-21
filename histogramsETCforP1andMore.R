########################################################################
# GoF.Simulations.Summary
# Given outputs of several GoF simulations
# (such as those obtained from a run of Estimate.p.Value.For.Testing),
# plot convergence plots of GoF statisitc and p-value..
#
# Input: 
#   - gof.values, a LIST OF LISTS? VECTOR OF LISTS? WHAT - list of values of the GoF statistic from a random walk, whose 1st entry is the OBSERVED ONE
#   - p.value.estimates, a LIST OF LISTS? VECTOR OF LISTS? WHAT - list of p-value esitmates from a random walk
# 
GoF.Simulations.Summary <- function(){
  # this is so that when we run 100 simulations, we get a mean p-value, 
  # and upper and lower quantiles for them, over the 100 simulations. 
  # clearly better than plotting 100 separate plots.
  print("to do.")
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
GoF.Testing.Plots <- function(gof.values,p.value.estimates, dataname="Set title w/ option 'dataname'", burn=0, filesave=FALSE,filename='GoF testing plots', grid=c(2,2)){
  
  print("This is still a little shitty b/c I don't check if input is of correct format..e.g., the two lists should be of same length if they come from the same simulation. For typical usage of the function, see function header @code. ")    
  
  # the following only makes sense if dataname has no special characters!! but i would have really preferred to set it this way: 
  #       if(filesave){pdf(paste('GoF testing plots for',dataname))}
  # otherwise i resort to 1 filename each time:
  if(filesave){
    pdf(filename) # open file connection
  }
  # plot in grid format; default = 2x2 grid
  par(mfrow = grid, mar=c(2,1,0,0)+3) # spacing; it goes c(bottom, left, top, right)  
  
  # 1) plot p-value estimates: 
  plot(p.value.estimates, main=dataname,sub=paste(length(p.value.estimates),"steps"),  xlab="Length of Walk", ylab="p-values", ylim=c(0,1), pch='.' )
  
  # 2) plot the sampling distribution of the GoF statistic:
  hist(gof.values,main=dataname, xlab="Goodness of Fit Statistic", ylab="frequency")
  abline(v=gof.values[1], col="red")
  
  # 3) MAYBE someone has burn-in:
  if(burn>0){
    p.estimates = Estimate.p.Value.From.GoFs(gof.values, burn) 
    plot(p.estimates[[2]],  main=dataname ,sub=paste(length(p.estimates[[2]]),"steps"),  xlab=paste("Length of Walk (after", burn,"burn-in steps)"), ylab="p-values", ylim=c(0,1), pch='.')    
    hist(gof.values[burn+2:length(gof.values)],main=dataname, xlab="Goodness of Fit Statistic", ylab="frequency")
    abline(v=gof.values[1], col="red")
  }
  
  if(filesave){
    print(paste("Plots are saved to",filename,"in the working directory (check with getwd())."))
    dev.off()  # close file connection.
  }
  # turn off grid printing:
  par(mfrow = c(1,1))
}
########
# - Runs Estimate.p.value.for.testing multiple times.
# - Records a summary of the p-value estimates in a text file.
# - Plots p-value convergence plots and gof histograms for each run both in a file and on for immediate viewing.
# - Calculates the quartiles from the progressive p-value estimats and plots these as well.  
########
Test.Goodness.Of.Fit<-function(gdir, gbidir=graph.empty(vcount(gdir),directed=FALSE), foldername, numSteps, iterations, mleMatr=NULL, model, ignore.trivial.moves=FALSE, tol=0.001, maxiter=100000, testname){
  if (ecount(gbidir)==0){
    mixed.graph = split.Directed.Graph(gdir)
    gdir = mixed.graph[[1]]
    gbidir = mixed.graph[[2]]  
  }
  
  if (is.null(mleMatr)) { mleMatr = Get.MLE(D.bay,B.bay, model, tol, maxiter) }
  
  if (ignore.trivial.moves){
    trivs.label = "no trivial moves"
    trivs.filename = ".no.trivial.moves"
    trivs.iteration = "non-trivial-move iteration"
  }else {
    trivs.label = ""
    trivs.filename = "."
    iteration = "iteration"
  }
  
  base.filename = sprintf("%s/%s.%s.%dsteps%s",foldername, testname, model, numSteps, trivs.filename)
  
  p.values = array(0,dim=c(iterations))
  p.progressive.estimates = array(0,dim=c(iterations,numSteps))
  gof.values = array(0,dim=c(iterations,numSteps+1))
  
  for (i in 1:iterations){
    cat(sprintf("iteration = %d\n",i))
    tmp = Estimate.p.Value.for.Testing(gdir, gbidir, steps=numSteps, model, ignore.trivial.moves, mleMatr)
    cat(sprintf("p.values[%d] = %f\n", i, tmp[[1]]))
    p.values[i] = tmp[[1]]
    num.moves = length(tmp[[2]])
    p.progressive.estimates[i,1:num.moves] = tmp[[2]]
    gof.values[i,1:(num.moves+1)] =  tmp[[3]]
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], filesave=TRUE, filename = sprintf("%s.iteration%d.figs.pdf",base.filename, i), grid=c(2,1), dataname = paste(testname,"; ", trivs.label," ", model))
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], grid=c(2,1), dataname=paste(testname,"; ", trivs.label, " ", model))
    
    save(p.values, file=sprintf("%s.p.values.RData",base.filename))
    save(p.progressive.estimates, file=sprintf("%s.p.progressive.estimates.values.RData",base.filename))
    save(gof.values, file=sprintf("%s.gof.values.values.RData",base.filename))
    
    cat(sprintf("mean(p.values[1:%d]) = %f | median(p.values[1:%d]) = %f | var(p.values[1:%d]) = %f\n", i, mean(p.values[1:i]), i, median(p.values[1:i]), i, var(p.values[1:i])))
  }
  filename=sprintf("%s.p.values.txt",base.filename)
  write(paste(testname, " ", model, " ", trivs.label), filename)
  write(sprintf("%d iterations",i), filename, append=TRUE)
  #####
  write(sprintf("p.values[1:%d]",i), filename, append=TRUE)
  write(p.values[1:i], filename, append=TRUE, ncolumns=i, sep = ", ")
  write(sprintf("p.values mean = %f\n median = %f\n var = %f", mean(p.values),median(p.values),var(p.values)), append=TRUE)
  #####
  
  qs=apply(p.progressive.estimates,FUN=quantile,2)
  pdf(sprintf("%s.p.value.quartiles.fig.pdf",base.filename))
  plot(qs[3,], ylab="p-value estimates",xlab="step", type="o",ylim=c(0,1),lwd=0.25, pch='.', main=paste(testname, trivs.label, model))
  lines(qs[2,], type="o", col="gray",lwd=0.25, pch='.')
  lines(qs[4,], type="o", col="gray",lwd=0.25, pch='.')
  dev.off()
  
  plot(qs[3,], ylab="p-value estimates",xlab="step", type="o",ylim=c(0,1),lwd=0.25, pch='.', main=paste(testname, trivs.label, model))
  lines(qs[2,], type="o", col="gray",lwd=0.25, pch='.')
  lines(qs[4,], type="o", col="gray",lwd=0.25, pch='.')
  
  return (list(p.values, p.progressive.estimates, gof.values, qs))
}

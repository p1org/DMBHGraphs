########
# Test.Model.Fit
# - Runs Estimate.p.value.for.testing multiple times.
# - Records a summary of the p-value estimates in a text file.
# - Plots p-value convergence plots and gof histograms for each run both in a file and on for immediate viewing.
# - Calculates the quartiles from the progressive p-value estimates and plots these as well.  
########
Test.Model.Fit<-function(gdir, gbidir=graph.empty(vcount(gdir),directed=FALSE), foldername, numSteps, iterations, mleMatr=NULL, model, ignore.trivial.moves=FALSE, tol=0.001, maxiter=100000, testname, plotlabel=NULL, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), beta.SBM.coin=c(1/2), SBM.blocks=NULL){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Test.Model.Fit error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else{
      if (!is.directed(gdir)){
        gbidir = gdir
        gdir = graph.empty(vcount(gdir), directed = TRUE)
      }
    }    
  }else{
    if (!is.directed(gdir))stop("Test.Model.Fit: First input must be a directed igraph object.")
    if (ecount(gbidir)==0){
      mixed.graph = split.Directed.Graph(gdir)
      gdir = mixed.graph[[1]]
      gbidir = mixed.graph[[2]]  
    }    
  }
  
  if (is.null(mleMatr)) { mleMatr = Get.MLE(gdir, gbidir, model, maxiter, tol,SBM.blocks=SBM.blocks) }
  
  if (ignore.trivial.moves){
    trivs.label = "no trivial moves"
    trivs.filename = ".no.trivial.moves"
    trivs.iteration = "non-trivial-move iteration"
  }else {
    trivs.label = ""
    trivs.filename = ""
    iteration = "iteration"
  }
  
  base.filename = sprintf("%s/%s.%s.%dsteps%s",foldername, testname, model, numSteps, trivs.filename)
  
  p.values = array(0,dim=c(iterations))
  p.progressive.estimates = array(0,dim=c(iterations,numSteps))
  gof.values = array(0,dim=c(iterations,numSteps+1))
  minNumSteps = numSteps
  
  if (is.null(plotlabel)){
    plotlabel = paste(testname, trivs.label, model)
  }
  for (i in 1:iterations){
    cat(sprintf("iteration = %d\n",i))
    tmp = Estimate.p.Value.for.Testing(gdir, gbidir, steps=numSteps, model, ignore.trivial.moves, mleMatr, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks=SBM.blocks)
    cat(sprintf("p.values[%d] = %f\n", i, tmp[[1]]))
    p.values[i] = tmp[[1]]
    if (length(tmp[[3]])<2){ stop("Please increase numSteps: The walk produced has less than two graphs; no results will be reported.")}
    num.moves = length(tmp[[2]])
    p.progressive.estimates[i,1:num.moves] = tmp[[2]]
    gof.values[i,1:(num.moves+1)] =  tmp[[3]]
    
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], filesave=TRUE, filename = sprintf("%s.iteration%d.figs.pdf",base.filename, i), grid=c(2,1), dataname = plotlabel)
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], grid=c(2,1), dataname=paste(testname, trivs.label, model))
    
    save(p.values, file=sprintf("%s.p.values.RData",base.filename))
    save(p.progressive.estimates, file=sprintf("%s.p.progressive.estimates.values.RData",base.filename))
    save(gof.values, file=sprintf("%s.gof.values.values.RData",base.filename))
    
    # Mcmc.Diagnostics(gof.values[i,1:num.moves+1],)
    
    cat(sprintf("mean(p.values[1:%d]) = %f | median(p.values[1:%d]) = %f | var(p.values[1:%d]) = %f\n", i, mean(p.values[1:i]), i, median(p.values[1:i]), i, var(p.values[1:i])))
    if (ignore.trivial.moves) 
      minNumSteps = min(num.moves,  minNumSteps)
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
  if (ignore.trivial.moves && minNumSteps>1){
    qs = qs[,1:minNumSteps]
  }
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


#### ================================================================================ ####
#### ================================================================================ ####

########
# Test.Model.Fit.Parallel
# - Same as Test.Model.Fit -- except it is set up to run iterations many parallel computations (for use on a cluster!)
# =========== NEW, UNTESTED METHOD ============== # 
########
Test.Model.Fit.Parallel<-function(gdir, gbidir=graph.empty(vcount(gdir),directed=FALSE), foldername, numSteps, iterations, mleMatr=NULL, model, ignore.trivial.moves=FALSE, tol=0.001, maxiter=100000, testname, plotlabel=NULL, ed.coin=c(1/3,1/3,1/3), nzconst.coin=c(ecount(gbidir)/(ecount(gdir)+ecount(gbidir)), ecount(gdir)/(ecount(gdir)+ecount(gbidir))), beta.SBM.coin=c(1/2), SBM.blocks=NULL){
  if (model == "beta.SBM"){
    if (is.null(SBM.blocks) || !is.vector(SBM.blocks))       
      stop("beta.SBM model requires a non-empty vector SBM.blocks input." )     
    else if (length(SBM.blocks)!=vcount(gdir))       
      stop("Test.Model.Fit error:\n beta.SBM model requires a vector SBM.blocks input of length equal to the number of vertices in the network." )
    else{
      if (!is.directed(gdir)){
        gbidir = gdir
        gdir = graph.empty(vcount(gdir), directed = TRUE)
      }
    }    
  }else{
    if (!is.directed(gdir))stop("Test.Model.Fit: First input must be a directed igraph object.")
    if (ecount(gbidir)==0){
      mixed.graph = split.Directed.Graph(gdir)
      gdir = mixed.graph[[1]]
      gbidir = mixed.graph[[2]]  
    }    
  }
  
  if (is.null(mleMatr)) { mleMatr = Get.MLE(gdir, gbidir, model, maxiter, tol, SBM.blocks=SBM.blocks) }
  
  if (ignore.trivial.moves){
    trivs.label = "no trivial moves"
    trivs.filename = ".no.trivial.moves"
    trivs.iteration = "non-trivial-move iteration"
  }else {
    trivs.label = ""
    trivs.filename = ""
    iteration = "iteration"
  }
  
  base.filename = sprintf("%s/%s.%s.%dsteps%s",foldername, testname, model, numSteps, trivs.filename)
  
  p.values = array(0,dim=c(iterations))
  p.progressive.estimates = array(0,dim=c(iterations,numSteps))
  gof.values = array(0,dim=c(iterations,numSteps+1))
  minNumSteps = numSteps
  
  if (is.null(plotlabel)){
    plotlabel = paste(testname, trivs.label, model)
  }
  
  
  # =================================== for cluster =============================================# 
  result <- foreach(i=1:iterations) %dopar% {
    #cat(sprintf("iteration = %d\n",i))
    tmp = Estimate.p.Value.for.Testing(gdir, gbidir, steps=numSteps, model, ignore.trivial.moves, mleMatr, SBM.blocks=SBM.blocks)
    tmp
  }
  # =================================== end for cluster =========================================# 
  
  for (i in 1:iterations){
    cat(sprintf("iteration = %d\n",i))
    tmp = result[[i]];
    # =================================== for cluster =============================================# 
    #    tmp = Estimate.p.Value.for.Testing(gdir, gbidir, steps=numSteps, model, ignore.trivial.moves, mleMatr, ed.coin, nzconst.coin, beta.SBM.coin, SBM.blocks=SBM.blocks)
    # =================================== end for cluster =========================================# 
    cat(sprintf("p.values[%d] = %f\n", i, tmp[[1]]))
    p.values[i] = tmp[[1]]
    if (length(tmp[[3]])<2){ stop("Please increase numSteps: The walk produced has less than two graphs; no results will be reported.")}
    num.moves = length(tmp[[2]])
    p.progressive.estimates[i,1:num.moves] = tmp[[2]]
    gof.values[i,1:(num.moves+1)] =  tmp[[3]]
    
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], filesave=TRUE, filename = sprintf("%s.iteration%d.figs.pdf",base.filename, i), grid=c(2,1), dataname = plotlabel)
    GoF.Testing.Plots(gof.values[i,1:(num.moves+1)], p.progressive.estimates[i,1:num.moves], grid=c(2,1), dataname=paste(testname, trivs.label, model))
    
    save(p.values, file=sprintf("%s.p.values.RData",base.filename))
    save(p.progressive.estimates, file=sprintf("%s.p.progressive.estimates.values.RData",base.filename))
    save(gof.values, file=sprintf("%s.gof.values.values.RData",base.filename))
    
    # Mcmc.Diagnostics(gof.values[i,1:num.moves+1],)
    
    cat(sprintf("mean(p.values[1:%d]) = %f | median(p.values[1:%d]) = %f | var(p.values[1:%d]) = %f\n", i, mean(p.values[1:i]), i, median(p.values[1:i]), i, var(p.values[1:i])))
    if (ignore.trivial.moves) 
      minNumSteps = min(num.moves,  minNumSteps)
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
  if (ignore.trivial.moves && minNumSteps>1){
    qs = qs[,1:minNumSteps]
  }
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

#### ================================================================================ ####
#### ================================================================================ ####
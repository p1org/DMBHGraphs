########################################################################
# Estimate.p.Value    												                          #
# Estimate the percentage of graphs in the fiber of D	                  #
# that are further from the MLE than G (that is, whose chi-square 
# statistic value is larger than that of G), for a model specified by the    
# user.                                                                 #
### ^^^^^ I WANT TO CHANGE THIS ONE LINE UP IN THE DESCIRPTION TO SPECIFY WE ARE USING A CHI-SQ STATISTIC AS GOF.

#
# to/do wish list: 
# 
# there should really be 1 fn (below) that crates plots out of 1 simulation
# and then
# another function (tbd) that plots simulation summaries over multiple runs. 
# 


########################################################################
#
#         ***** IN PROGRESS ******* IN PROGRESS *****  IN PROGRESS *****
#
# GoF.Testing.Plots  
# Given a random walk, e.g., output of a run of Estimate.p.Value.For.Testing, 
#...what I really want is input 2 things: list of gof values and pvalues; don't assume output of a fn.
# plot histogram of GoF statisitcs and p-values for the random walk.
# 
# Input: 
#   - gof.values, a list of values of the GoF statistic from a random walk, whose 1st entry is the OBSERVED ONE
#   - p.values, a list of p-value esitmates from a random walk
#	Optional input:                                                       
#   - filesave, a Boolean indicating whether to save plots to pdf file
#   - filename, 
#   - burn, integer number of burn-in steps; 0 default
#   - grid, vector c(i,j) indicating plots to be made on an ixj grid
#   - .....[list of other things one may want to plot] - UNFINISHED
# Output:                                                               
#   - plot
# Typical usage of this function:
#   fiber.walk=Estimate.p.Value.for.Testing(D,B,steps.for.walk=10)
#   gof.values = fiber.walk[[3]]
#   p.values = fiber.walk[[2]]  
#   GoF.Testing.Plots(gof.values,p.values,dataname="test data set",filesave=FALSE)
########################################################################
GoF.Testing.Plots <- function(gof.values,p.values, dataname="Set title w/ option 'dataname'", burn=0, filesave=FALSE,filename='GoF testing plots', grid=c(2,2)){
  
  print("This is still a little shitty b/c I don't check if input is of correct format..e.g., the two lists should be of same length if they come from the same simulation. For typical usage of the function, see function header @code. ")    
  
  # the following only makes sense if dataname has no special characters!!  BUT I REALLY PREFER IT THIS WAY!! 
  # SO - TO DO - ADD CHECKS AND MAKE THIS WORK: 
  #       if(filesave){pdf(paste('GoF testing plots for',dataname))}
  # otherwise i resort to 1 filename each time:
  if(filesave){
    pdf(filename) # open file connection
  }
  # plot in grid format; default = 2x2 grid
  par(mfrow = grid, mar=c(2,1,0,0)+3) # spacing; it goes c(bottom, left, top, right)  
  
  # 1) plot p-value estimates: 
  plot(p.values, main=dataname,sub=paste(length(p.values),"steps"),  xlab="Length of Walk", ylab="p-values", ylim=c(0,1))
  
  # 2) plot the sampling distribution of the GoF statistic:
  hist(gof.values,main=dataname, xlab="Goodness of Fit Statistic", ylab="frequency")
  abline(v=gof.values[1], col="red")
  
  # 3) MAYBE someone has burn-in:
  if(burn>0){
    p.estimates = Estimate.p.Value.From.GoFs(gof.values, burn) 
    plot(p.estimates[[2]],  main=dataname ,sub=paste(length(p.estimates[[2]]),"steps"),  xlab=paste("Length of Walk (after", burn,"burn-in steps)"), ylab="p-values", ylim=c(0,1))    
  }
  
  if(filesave){
    print(paste("Plots are saved to",filename,".pdf."))
    dev.off()  # close file connection.
  }
}

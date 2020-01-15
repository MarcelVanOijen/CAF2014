## INITIALISE SITE
   source('initialisation/initialise_CAF2014_Turrialba_1.R')

## RUN MODEL
   output <- run_model()

## PRINTING AND PLOTTING ##
   print(output[2406,])
   plot_output()

################################################################################
# Additional processing of the outputs
################################################################################

# The model is written in FORTRAN, but we call it from R. Usually when we call
# the model from R, we store the results in a matrix. Above we saw an example
# where that matrix is called 'output'. We are now going to do some
# calculations with that matrix.

# The matrix is quite big. The following R-statement shows how big:
   dim(output)
# The Number of rows is equal to NDAYS (the length of the simulated period in
# days), and the number of columns is equal to NOUT (the number of output
# variables that FORTRAN calculates):
   cat('NDAYS=',NDAYS,'and NOUT=',NOUT)
# The names and units of the output variables are stored in arrays called
# 'outputNames' and 'outputUnits', respectively. The following command shows them
# side by side:
   data.frame(outputNames,outputUnits)
# We see that the variable SA, which is the shaded fraction of the land, is
# in column 4, and the coffee LAI of the sun and shade parts of the land are in
# columns 10 and 11, respectively.
# So we can calculate the overall spatial average of the coffee LAI as folllows:
   SA        <- output[,4]
   LAI_sun   <- output[,7]
   LAI_shade <- output[,8]
   LAI       <- (1-SA)*LAI_sun + SA*LAI_shade   
# Let's plot these three variables in one graph against time (column 1 of 'output'):
   time      <- output[,1]
   plot  (time,LAI_sun  ,col=1,ylab='LAI')
   points(time,LAI_shade,col=2)
   points(time,LAI      ,col=3)
   legend('bottomright',legend=c('sun','shade','field'),lty=1,lwd=3,col=1:3)
# Now we want to calculate a moving average of LAI. Let's first define an R-function
# for calculating moving averages:
   ma <- function(x,n=365){filter(x,rep(1/n,n), method="convolution",sides=1)}
# Let's now plot the (spatially) average LAI that we calculated above plus
# monthly and annual moving averages of it:
   plot  (time,   LAI     ,col=1,ylab='LAI')
   points(time,ma(LAI, 30),col=2)
   points(time,ma(LAI,365),col=3)
   legend('bottomright',legend=c('field','monthly av.','annual av.'),lty=1,lwd=3,col=1:3)
# Now let's examine the harvests, which are in columns 12 and 13.
   harvCP_sun   <- output[, 9]
   harvCP_shade <- output[,10]
# Let's again calculate the field averages and plot:
   harvCP       <- (1-SA)*harvCP_sun + SA*harvCP_shade   
# Let's plot these three variables in one graph:
   plot  (time,harvCP_sun  ,col=1,pch=19,type='p',ylab='harvCP')
   points(time,harvCP_shade,col=2,pch=19)
   points(time,harvCP      ,col=3,pch=19)
   legend('left',legend=c('sun','shade','field'),pch=19,col=1:3)
# Now we want to calculate annual totals of harvCP. Say we want to calculate the
# annual totals only at the end of each calendar year, which are given in the second
# output column. Our simulation runs over 7 calendar years, so we want a list of 7
# total harvests. One way of doing the calculation is as follows:
   year              <- output[,2]
   years             <- unique( year )
   harvCP_annualsums <- sapply( years, function(y){sum(harvCP[year==y])} )
   data.frame(years,harvCP_annualsums)
     
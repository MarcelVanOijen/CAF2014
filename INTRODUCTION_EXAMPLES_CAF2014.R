#                       CAF2014: INTRODUCTION AND EXAMPLES
#                              MvO (2020-01-15)

################################################################################
### 0. Introduction
################################################################################
#  This file shows how to run the CAF2014 model (v. 2014-07-30), inspect output,
# change parameters, and do a Bayesian calibration (BC).
#  If this file is opened in R, you can run all lines of code (i.e. the lines
# that do not begin with '#' which are comment lines) straight away.

# The model is operated from R, but the CAF2014-code itself is in FORTRAN.
# Parameters, weather and management (fertilisation, pruning of coffee and
# trees, thinning of trees) are all defined in R and then they are given as
# input to the main model subroutine called 'CAF2014', which we find in file
# 'CAF2014.f90'.

# Several R-functions have been defined that make working with the model easy.
# The most important function is 'run_model()', which is the R-function that
# runs CAF2014 itself. Examples are given in section 1. below. But there are
# several other routines (for plotting, making tables, sensitivity analysis)
# that we'll explain below as well.

# One important thing to realise is that at the beginning of an R-session,
# R begins with a clean slate, i.e. it does not know the modelling functions
# that we defined. So we must always begin by running a so-called
# 'initialisation file' which will make the functions known to R. We include
# examples of such site-specific initialisation files, with names like
# 'initialise_CAF2014_Turrialba_1.R' which can be used for that purpose, but
# you may want to add files for your own sites.

# Each of the site-specific initialisation files includes a call to the R-script
# 'initialisation_CAF2014_general.R' where the modelling functions are defined.
# The site-specific initialisation files are also the place where we set the
# parameter values, weather data and management regines for the runs that we are
# going to do.

# So, the standard way of working with CAF2014 involves at least two steps:
#   1. run an initialisation file,
#   2. use R-functions to run the model and do analyses.

# The rest of this file gives examples of working with this set-up.

################################################################################
### 1. Initialise and run the model for a site
################################################################################

# We initialise the model to run for Turrialba 1998-2004.

  source("initialisation/initialise_CAF2014_Turrialba_1.R")

# This will define the seven inputs to the model:
#   (1) parameter values,
#   (2) weather data,
#   (3) calendar for fertilisation,
#   (4) calendar for pruning of coffee,
#   (5) calendar for pruning of trees,
#   (6) calendar for thinning of trees,
#   (7) length of simulation period.
# These inputs will be known to R as: (1) 'params', (2) 'matrix_weather',
# (3) 'calendar_fert', (4) 'calendar_prunC', (5) 'calendar_prunT', (6)
# 'calendar_thinT' and (7) 'NDAYS'.

# We run CAF2014 by calling the function 'run_model'.
# We run the model with the above seven inputs, and put the results in "output":

  output <- run_model( p     = params,
                       w     = matrix_weather,
          		  			 calf  = calendar_fert,
			  	          	 calpC = calendar_prunC,
				  	           calpT = calendar_prunT,
					             caltT = calendar_thinT,
                       n     = NDAYS )

# If we do not change the order of the inputs, the "p=" etc. are not needed:

  output <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
	            				 calendar_prunT, calendar_thinT, NDAYS )

# By default, the 'run_model' function expects arguments called 'params',
# 'matrix_weather', 'calendar_fert' ... 'NDAYS', so in this case we could
# have left those out too, and still got the same output!

  output <- run_model()

# We can plot the output of CAF2014 on screen:

  plot_output(output)

# By default, the 'plot_output' function expects its first argument to
# be called 'output' so we could have shortened the last command:

  plot_output()

# We can send the plot to a pdf-file instead of the screen as follows.
# [We'll use the filename "output.pdf" but any other name is OK too]

  pdf( "output.pdf" )
    plot_output()
  dev.off()

# We can also request a txt-file with some tabulated results:

  table_output()

# That command will place a new file in the root directory, named output[..].txt,
# which you can examine in EXCEL for example.

# If we only want plotted or tabulated output for specific variables, we can
# name those variables after the 'vars'-argument of the functions:

  pdf( "example_output_LAIT_harvDMav.pdf" )
    plot_output ( vars=c("LAIT","harvDMav_year") )
  dev.off()
  table_output( vars=c("LAIT","harvDMav_year"),
                file_table="example_output_LAIT_harvDMav.txt" )

################################################################################
### 2. Comparing the output from multiple runs
################################################################################
# If you run the model multiple times, e.g. with different parameter values or
# harvest times, then the outputs from the different runs can easily be compared
# with each other.

  source("initialisation/initialise_CAF2014_Turrialba_1.R")
  output_shade   <- run_model()
  source("initialisation/initialise_CAF2014_Turrialba_2.R")
  output_noshade <- run_model()
  plot_output( list(output_shade,output_noshade) )

# The example shows that if you put the outputs from different runs in one
# list, then the 'plot_output()' function knows how to plot the results in one
# graph.

################################################################################
### 3. Selecting a different set of model output variables
################################################################################
# A common change is adding new output variables to the model. The list of
# output variables produced by the model can be seen in two different files:
#   'initialisation/initialisation_CAF2014_general.txt'
#   'model/CAF2014.f90'.
# MAKE SURE THAT THOSE TWO FILES STAY CONSISTENT!
# And remember to recompile if you change the two files, because one is a
# FORTRAN file. Recompiling is done simply by running the file
# 'compile_CAF2014.bat'. That will create a new 'CAF2014.DLL'.

################################################################################
### 4. Changing things in the model: how do we do that in general?
################################################################################
# The general idea is always to copy and modify one of the existing files to
# do anything different. If you change any of the FORTRAN files (the ones with
# extension ".f90"), the program needs to be compiled again. Just run the bat-
# file 'compile_CAF2014.bat' to accomplish that.

################################################################################
### 5. Changing parameter values
################################################################################
# Parameters can be changed in different ways. First, you can change or add a
# column in 'parameters/parameters_default.txt'. Once you've added a column,
# you can refer to that column in the initialisation file. See how different
# parameter columns (in the code: 'parcol') are specified in the files
#   'initialisation/initialise_CAF2014_Turrialba_1' and
#   'initialisation/initialise_CAF2014_Turrialba_2'.
# A second way would be to initialise the model as normal, using an
# initialisation file and then modify 'params' in R-Studio itself. We now show
# how that can be done. Let's begin as always by running an initialisation file.

  source("initialisation/initialise_CAF2014_Turrialba_1.R")

# That command sets all parameter values, which we can check as follows:

  print(params)

# Say we want to vary parameter 'KEXT'. First we need to find out which number
# it has, i.e. is it params[1] or params[2] or ...? We can check parameter
# numbering in file 'model/set_params.f90', but we can also look it up as
# follows:

  i_KEXT <- match( 'KEXT', row.names(df_params) ) ; print(i_KEXT)

# Now we can, for example, run the model with the default value of KEXT and half
# that value:

  output_default <- run_model()
  params[i_KEXT] <- 0.5 * params[i_KEXT]
  output_KEXT_50 <- run_model()

# The two sets of model outputs can now be compared:

  plot_output( list(output_default,output_KEXT_50) )

# However, the third and simplest way of studying parameter changes is to
# use the 'sensitivity analysis' function SA(), which is explained in the
# following section.

################################################################################
### 6. Sensitivity analysis
################################################################################

  source("initialisation/initialise_CAF2014_Turrialba_1.R")

# If we want to see the impact of changing a single parameter, we can
# use the 'SA' function. We tell SA by how much the parameter should be
# multiplied in the different runs. For example:

  SA( "RAINMULT", c(1,0.5,0.25) )

# The results of this sensitivity analysis are written to two files,
# one with plots (called 'SA_[].pdf'), and one with a table ('SA_[].txt').

# Let's use a different parameter:

  SA( "KEXT", c(0.5,1,2) )

# By default, the 'SA' function varies parameter "KEXT", and by default
# it also uses multiplication values 0.5, 1 and 2. So we get the same with:

  SA()

# A full specification of the 'SA()' function, using all the defaults (so it
# gives the same results as above), is as follows:

  pmult     <- 2^(-1:1)
  vars      <- outputNames[-(1:1)]
  nrow_plot <- ceiling( sqrt((length(vars)+1) * 8/11) )
  SA( parname_SA = "KEXT",
      pmult      = pmult,
      vars       = vars,
      leg_title  = parname_SA,
      nrow_plot  = nrow_plot,
      ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
      lty        = rep(1,length(pmult)),
      lwd        = rep(3,length(pmult)),
      file_init  = "initialisation/initialise_CAF2014_Turrialba_1.R" )

# So the function can take nine arguments, which we can change as we see fit.
# For example, we can reduce the number of output variables:

  SA( "RAINMULT", vars=c("LAI(1)","LAI(2)","LAIT","harvDMav_year") )

# or:

  SA( "RAINMULT", vars="LAIT")

################################################################################
### 7. BUILDING UP A LIBRARY OF INFORMATION ON PARAMETERS & SITES
################################################################################
# The file 'parameters_default.txt', together with the directory
# 'initialisation', can be seen as the heart of the system, where you can
# gradually store more and more information about different sites and/or
# treatments. Columns can be added to the parameters file, for example if
# a new shade tree species is introduced. The two columns with parameter
# values that are currently in that file are not to be taken too seriously.
# Each initialisation file points to one column in parameters_default.txt but
# also includes other (non-parameter) information, e.g. about management.

################################################################################
### 8. Bayesian Calibration (BC)
################################################################################

# BC is carried out by means of Markov Chain Monte Carlo sampling (MCMC), using
# the Metropolis algorithm. We only give a brief introduction here to the
# relevant files.

# An example of a single-site BC is the following. If you just want to see a
# short first test, make sure that 'nChain' (which is the number of iterations
# in the MCMC) is set to a low number like 1000. You set the value of nChain in
# file 'in file 'BC/BC_CAF2014_MCMC_init_Turrialba_1').

  source("BC_CAF2014_Turrialba_1.R")

# The results of the BC are written to four files:
# 1. 'CAF2014_parModes_[].txt': parameter values
# 2. 'BC_parameters_traceplots_[].pdf': parameter trace plots
# 3. 'BC_parameters_priorbeta_histograms_[].pdf': parameter distributions
# 4. 'BC_outputs_data_[].pdf': model outputs and the calibration data.

# Multi-site BC can be done as in the following example:

  source("BC_CAF2014_Turrialba_1&2.R")

# Note that it is now possible for every parameter to specify its own degree of
# 'site-specificity'. That is done in the last column in the file
# 'parameters_BC_[].txt'. There you can specify whether the parameter should be
# calibrated generically for all of the sites, or that it should get different
# values for different subsets of the sites.

# After the BC, we can run the model for the 'Maximum A Posteriori' (MAP)
# parameter vector and do a sensitivity analysis.
# We first find the name of the parameter values file that was produced last.
# [CAREFUL: we only included the hour&minute time in the file names, and not
# the day. So the next two lines only look at the hour-minute time, and not
# the day-number, to determine which file is the most recent one!]

  files_parModes <- list.files( pattern="CAF2014_parModes" )
  file_parModes  <- tail( files_parModes, 1 )

# Now we do the SA, starting from the MAP parameter vector

  SA_BC( "KEXT", file_init_BC = "BC/BC_CAF2014_MCMC_init_Turrialba_1&2.R",
                 file_par     = file_parModes,
                 partype      = "MAP" )

# The 'SA_BC' function produces three files:
# 1. 'SA_BC_outputs_[].pdf': model outputs for all variables.
# 2. 'SA_BC_outputs_data_[].pdf': model outputs for observed variables together
#     with the calibration data.
# 3. 'SA_BC_likelihoods_[].pdf': impact of parameter changes on likelihoods
# Note that SA_BC() does the sensitivity analysis for each of the sites that
# was included in the BC. So if the BC was for 2 different sites, SA_BC()
# will produce results for both of those sites, and it even takes into account
# that some parameters are site-specific and others are generic.

################################################################################
### 9. Single run of CAF2014 using a parameter vector from previous BC
################################################################################
# Each Bayesian calibration produces a txt-file with parameter vectors. You can
# pick out one of those vectors and run the model for it. There are different
# ways of doing that. The file 'run_CAF2014_BC_parMode.R' shows two.

################################################################################
### 10. How to analyse the output
################################################################################
# Once we have run the model, the output can be processed in R as you see fit.
# Some examples are in the file
# 'run_CAF2014_Turrialba_1_AdditionalProcessingOutput.R'.

################################################################################
### 11. How to add parameters
################################################################################
# If you change the model and include new parameters, the model needs to be told
# the names and values of the parameters. Three files need to be changed:
# 1. 'declare_parameters.f90': include the new parameter(s) where you want
# 2. 'set_params.f90'        : add the parameter(s) at the bottom
# 3. 'parameters_default.txt': add the parameter(s) at the bottom
# And remember to recompile the model, because you have changed Fortran files.

################################################################################
### 12. Sink strength of coffee beans - new parameters: FSINKPMAX0, KSINKPMAX
################################################################################
# CAF2014 has two parameters that regulate coffee bean sink strength that both
# were not present in CAF2007: FSINKPMAX0 and KSINKPMAX.

# Giving the parameter FSINKPMAX0 a lower value than 1 has the effect of
# reducing sink strength and yield of coffee in the first year of production
# (i.e. when the simulation DAY > DAYSPLNOP).
# The parameter FSINKPMAX0 has been given a default value of 0.2 (in file
# 'parameters_default.txt'), i.e. significantly reducing the yield in the first
# productive year.

# The parameter KSINKPMAX can be used to increase biannual variation in coffee
# yield. The default parameter value is 0.8, which in most cases will lead to
# big yield-swings from year to year.

# The behaviour of the model seems more realistic than before (in CAF2007) if we
# set FSINKPMAX0 = 0.2 and KSINKPMAX = 0.8, but the parameters should be
# properly calibrated.

################################################################################
### 13. Triggering of flowering: a new parameter, RAINdoyHI
################################################################################
# CAF2014 simulates flowering in a slightly different way from CAF2007.
# Flowering is now initiated on the first day of the year where the product of
# rainfall and doy (the Julian day number) exceeds the value of the parameter
# RAINdoyHI. The default value of RAINdoyHI (as set in 'parameters_default.txt')
# is 1000 mm. The parameter needs to be calibrated.

# Note that this new algorithm for flowering is not very mechanistic: we would
# prefer an algorithm that does not use a function of doy, but of the degree of
# water stress experienced by the coffee plants.

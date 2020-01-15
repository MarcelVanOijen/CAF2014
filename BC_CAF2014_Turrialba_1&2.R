## 1. INITIALISE MCMC ##
   source('BC/BC_CAF2014_MCMC_init_Turrialba_1&2.R')
   nChain <- as.integer(2000) ; source('BC/BC_CAF2014_MCMC_init_general.R')

## 2. RUNNING THE MCMC ##
   source('BC/BC_CAF2014_MCMC.R')

## 3. PRINTING AND PLOTTING ##
   source('BC/BC_export_parModes.R')
   source('BC/BC_plot_parameters_traceplots.R')
   source('BC/BC_plot_parameters_priorbeta_histograms.R')
   source('BC/BC_plot_outputs_data.R')
   
## 4. SAVING WORKSPACE
#    imagefilename <- paste( "BC_Turrialba_1&2",
#                            format(Sys.time(),"%H_%M.Rdata"), sep="" )
#    save.image(file=imagefilename)
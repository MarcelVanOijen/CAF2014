### Running the model with the MAP from a previous BC

################################################################################
### DEFINE SITE and PARAMETER VECTOR
################################################################################
# Name the initialisation file:
   file_initialisation <- 'initialisation/initialise_CAF2014_Turrialba_1.R'
# Find the file with the desired parameter values. For example:
   files_parModes      <- list.files( pattern="CAF2014_parModes" )
   file_parModes       <- tail( files_parModes, 1 )
# Name the column in the parameter file with the parameter values you'll use.
# We here select the MAP in column 2:
   col_params          <- 2

################################################################################
### RUN MODEL with the selected MAP parameter vector: METHOD 1
################################################################################
## INITIALISE SITE
   source(file_initialisation)
## MODIFY PARAMETERS ACCORDING TO ONE OF THE PARAMETER MODES FROM THE BC
   df_params       <- read.table( file_parModes, header=T, sep="\t", row.names=1 )
   parcol          <- col_params
   params_selected <- df_params[,parcol]
## RUN MODEL AND PLOT THE OUTPUT
   output          <- run_model( p=params_selected )
   plot_output(output)

################################################################################
### RUN MODEL with the selected MAP parameter vector: METHOD 2
################################################################################
SA_BC(pmult         = 1,
      file_init_BC  = "BC/BC_CAF2014_MCMC_init_Turrialba_1.R",
      file_par      = file_parModes,
      partype       = "MAP" )

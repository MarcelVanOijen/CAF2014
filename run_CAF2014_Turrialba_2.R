## INITIALISE SITE
   source('initialisation/initialise_CAF2014_Turrialba_2.R')
   
## RUN MODEL
   output <- run_model()

## PRINTING AND PLOTTING ##
   print(output[2406,])
   plot_output()

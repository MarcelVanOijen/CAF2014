## BC_CAF2014_MCMC_init_Turrialba_1.R ##

## MCMC chain length
   nChain        <- as.integer(1000)

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior    <- "parameters/parameters_BC_Turrialba_1&2.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')
   
## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- c("initialisation/initialise_CAF2014_Turrialba_1.R",
                               "initialisation/initialise_CAF2014_Turrialba_2.R" )
   sitedata_filenames     <- c("data/data_Turrialba_1.txt",
                               "data/data_Turrialba_2.txt" )
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

   source(sitesettings_filenames[1]) # Run model to determine NOUT (number of output variables)
   list_cv_var <- sitelist
# Global settings of measurement uncertainty (all sites and variables):  
   cv_default  <- 0.3
# Variable-specific settings of measurement uncertainty:
   for (s in 1:nSites) {
     list_cv_var[[s]]                  <- rep( cv_default, NOUT )
     names(list_cv_var[[s]])           <- outputNames
     list_cv_var[[s]]["harvDMav_year"] <- 0.1
   }
# Measurement uncertainty that is both site- and variable-specific:
     list_cv_var[[1]]["WA(1)"] <- 0.1 ; list_cv_var[[2]]["WA(1)"] <- 0.2
   
## PROPOSAL TUNING FACTOR  
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average step length
                        # (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2014_MCMC_init_general.R')

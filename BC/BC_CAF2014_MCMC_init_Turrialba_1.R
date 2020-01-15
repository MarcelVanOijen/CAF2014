## BC_CAF2014_MCMC_init_Turrialba_1.R ##

## MCMC chain length
   nChain        <- as.integer(1000)

## FILE FOR PRIOR PARAMETER DISTRIBUTION
   file_prior    <- "parameters/parameters_BC_Turrialba_1.txt"

## LIKELIHOOD FUNCTION ##
   source('BC/fLogL_Sivia.R')
   
## SETTINGS FOR THE DIFFERENT CALIBRATION SITES (at least one site)
   sitesettings_filenames <- "initialisation/initialise_CAF2014_Turrialba_1.R"
   sitedata_filenames     <- "data/data_Turrialba_1.txt"
   nSites                 <- length(sitedata_filenames)
   sitelist               <- list() ; length(sitelist) <- nSites

   source(sitesettings_filenames[1]) # Run model to determine NOUT (number of output variables)
   list_cv_var <- sitelist
   cv_default  <- 0.3
   for (s in 1:nSites) {
     list_cv_var[[s]]                  <- rep( cv_default, NOUT )
     names(list_cv_var[[s]])           <- outputNames
     list_cv_var[[s]]["h"            ] <- 0.1
     list_cv_var[[s]]["LAI(1)"       ] <- 0.1
     list_cv_var[[s]]["LAI(2)"       ] <- 0.1
     list_cv_var[[s]]["harvDMav_year"] <- 0.1
     list_cv_var[[s]]["harvCP(2)"    ] <- 0.1
   }
   
## PROPOSAL TUNING FACTOR  
   fPropTuning   <- 0.2 # This factor is used to modify Gelman's suggested average step length
                        # (2.38^2 / np_BC) which seems too big

## GENERAL INITIALISATION FOR MCMC
   source('BC/BC_CAF2014_MCMC_init_general.R')

  source("initialisation/initialise_CAF2014_Turrialba_1.R")
  output <- run_model( p     = params,
                       w     = matrix_weather,
          		  			 calf  = calendar_fert,
			  	          	 calpC = calendar_prunC,
				  	           calpT = calendar_prunT,
					             caltT = calendar_thinT,
                       n     = NDAYS )
  plot_output()
  
  source("initialisation/initialise_CAF2014_Turrialba_1.R")
  output_shade   <- run_model()
  source("initialisation/initialise_CAF2014_Turrialba_2.R")
  output_noshade <- run_model()
  plot_output( list(output_shade,output_noshade) )

  print(params)

  SA( "RAINMULT", c(1,0.5,0.25) )

  source("BC_CAF2014_Turrialba_1&2.R")

  files_parModes <- list.files( pattern="CAF2014_parModes" )
  file_parModes  <- tail( files_parModes, 1 )
  SA_BC( "KEXT", file_init_BC = "BC/BC_CAF2014_MCMC_init_Turrialba_1&2.R", 
                 file_par     = file_parModes,
                 partype      = "MAP" )

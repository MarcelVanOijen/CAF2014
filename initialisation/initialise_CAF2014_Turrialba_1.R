## initialise_CAF2014_Turrialba_1.R ##

## 1. SITE-SPECIFIC SETTINGS ##
   year_start     <- as.integer(1998)
   doy_start      <- as.integer( 150)
   NDAYS          <- as.integer(2406)
   file_weather   <- 'weather/weather_98_04_WITH_HEADER_LINE.txt'
   file_params    <- 'parameters/parameters_default.txt'
     parcol       <- 1

## 2. GENERAL INITIALISATION ##
   MODEL_dll      <- 'CAF2014.DLL'
   dyn.load( MODEL_dll )
   source('initialisation/initialise_CAF2014_general.R')

## 3. WEATHER
   matrix_weather <- read_weather_CAF( year_start, doy_start, NDAYS, file_weather )
	
## 4. PARAMETERISATION AND MANAGEMENT ##
   df_params      <- read.table( file_params, header=T, sep="\t", row.names=1 )
   params         <- df_params[,parcol]
   
   calendar_fert [1:21,1] <- rep( 1998:2004, each=3 )
   calendar_fert [1:21,2] <- rep( c(14,135,227), 7 )
   calendar_fert [1:21,3] <- 50
#    calendar_prunC[1   , ] <- c(2000,182,0.95)
#    calendar_prunC[2   , ] <- c(2002,182,0.95)
#    calendar_prunC[3   , ] <- c(2004,182,0.95)
   calendar_prunT[1: 7,1] <- 1998:2004
   calendar_prunT[1: 7,2] <- 182
   calendar_prunT[1: 7,3] <- 0.5
   calendar_thinT[1   , ] <- c( 2000,270, 0.5 )
   
## 5. CREATE EMPTY MATRIX y FOR MODEL OUTPUT ##
   y              <- matrix(0,NDAYS,NOUT)

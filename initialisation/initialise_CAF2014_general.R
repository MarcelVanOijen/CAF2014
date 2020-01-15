### initialise_CAF2014_general.R ##

################################################################################
calendar_fert  <- matrix( -1, nrow=100, ncol=3 )
calendar_prunC <- matrix( -1, nrow=100, ncol=3 )
calendar_prunT <- matrix( -1, nrow=100, ncol=3 )
calendar_thinT <- matrix( -1, nrow=100, ncol=3 )

################################################################################
### 1. MODEL LIBRARY FILE & FUNCTION FOR RUNNING THE MODEL
################################################################################
run_model <- function(p     = params,
                      w     = matrix_weather,
					  calf  = calendar_fert,
					  calpC = calendar_prunC,
					  calpT = calendar_prunT,
					  caltT = calendar_thinT,
                      n     = NDAYS) {
  .Fortran('CAF2014', p,w,calf,calpC,calpT,caltT,n,NOUT,
                      matrix(0,n,NOUT))[[9]]
}

################################################################################
### 2. FUNCTION FOR READING WEATHER DATA
################################################################################
read_weather_CAF <- function(y = year_start,
                             d = doy_start,
                             n = NDAYS,
                             f = file_weather) {
  df_weather            <- read.table( f, header=TRUE )
  row_start             <- 1
  while( df_weather[row_start,]$year< y ) { row_start <- row_start+1 }
  while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
  df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
  NMAXDAYS              <- as.integer(10000)
  NWEATHER              <- as.integer(8)
  matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
  matrix_weather[1:n,1] <- df_weather_sim$year
  matrix_weather[1:n,2] <- df_weather_sim$doy
  matrix_weather[1:n,3] <- df_weather_sim$GR
  matrix_weather[1:n,4] <- df_weather_sim$TMIN
  matrix_weather[1:n,5] <- df_weather_sim$TMAX
  matrix_weather[1:n,6] <- df_weather_sim$VP
  matrix_weather[1:n,7] <- df_weather_sim$WN
  matrix_weather[1:n,8] <- df_weather_sim$RAIN   
  return(matrix_weather)
}
   
################################################################################
### 3. OUTPUT VARIABLES
################################################################################
outputNames <- c(
  "Time"    , "year"    , "doy"      ,
  "SA"      , "h"       , "LAIT"     ,
  "LAI(1)"  , "LAI(2)"  , "harvCP(1)", "harvCP(2)", "harvDMav_year",
  "WA(1)"   , "WA(2)"   , "treedens" )
  
outputUnits <- c(
  "(y)"     , "(y)"     , "(d)"      ,
  "(-)"     , "(m)"     , "(m2 m-2)" ,
  "(m2 m-2)", "(m2 m-2)", "(kgC m-2)", "(kgC m-2)", "(t DM ha-1)"  ,
  "(mm)"    , "(mm)"    , "(m-2)"    )
NOUT <- as.integer( length(outputNames) )
   
################################################################################
### 4. FUNCTIONS FOR EXPORTING THE RESULTS TO FILE (pdf with plots, txt with table)
################################################################################
plot_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:3)],
  leg         = paste( "Run", 1:length(list_output) ),
  leg_title   = "LEGEND",
  nrow_plot   = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot   = ceiling( (length(vars)+1)/nrow_plot ),
  lty         = rep(1,length(list_output)),
  lwd         = rep(3,length(list_output))
) {
  par( mfrow=c(nrow_plot,ncol_plot), mar=c(2, 2, 2, 1) )
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  for (iv in 1:nvars) {
    c       <- col_vars[iv]
    g_range <- range( sapply( 1:nlist, function(il){range(list_output[[il]][,c])} ) )
    plot( list_output[[1]][,1], list_output[[1]][,c],
          xlab="", ylab="", cex.main=1,
          main=paste(outputNames[c]," ",outputUnits[c],sep=""),
          type='l', col=1, lty=lty[1], lwd=lwd[1], ylim=g_range )
    if (nlist >= 2) {
      for (il in 2:nlist) {
      points( list_output[[il]][,1], list_output[[il]][,c],
              col=il, type='l', lty=lty[il], lwd=lwd[il] )            
      }
    }
    if ( (iv%%(nrow_plot*ncol_plot-1)==0) || (iv==nvars) ) {
      plot(1,type='n', axes=FALSE, xlab="", ylab="")
      legend("bottomright", leg, lty=lty, lwd=lwd, col=1:nlist, title = leg_title)
    }
  }
}

table_output <- function(
  list_output = list(output),
  vars        = outputNames[-(1:1)],
  file_table  = paste( "output_", format(Sys.time(),"%H_%M.txt"), sep="" ),
  leg         = paste( "Run", 1:length(list_output) )
) {
  if (!is.list(list_output)) list_output <- list(list_output) ; nlist <- length(list_output)
  col_vars <- match(vars,outputNames)                         ; nvars <- length(vars)
  table_output         <- c("day", list_output[[1]][,1:1] )
  for (il in 1:nlist) {
    table_il     <- if (nvars==1) c    (vars, list_output[[il]][,col_vars]) else
                                  rbind(vars, list_output[[il]][,col_vars])
    table_output <- cbind( table_output, table_il ) 
  }
  colnames(table_output) <- c( "",rep(leg,each=nvars) )
  write.table( table_output, file_table, sep="\t", row.names=F )
}
   
################################################################################
### 5. FUNCTIONS FOR ANALYSIS

#######################
### 5.1 Function 'SA()'
#######################
SA <- function( parname_SA = "KEXT",
                pmult      = 2^(-1:1),
                vars       = outputNames[-(1:1)],
                leg_title  = parname_SA,
                nrow_plot  = ceiling( sqrt((length(vars)+1) * 8/11) ),
                ncol_plot  = ceiling( (length(vars)+1)/nrow_plot ),
                lty        = rep(1,length(pmult)),
                lwd        = rep(3,length(pmult)),
                file_init  = "initialisation/initialise_CAF2014_Turrialba_1.R",
                file_plot  = paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.pdf"),sep="")
) {
  source(file_init)
  cat( "SA initialised for:", substr(basename(file_init),1,nchar(basename(file_init))-2), "\n")
  ip_SA          <- match( parname_SA, row.names(df_params) )
  par_SA_default <- params[ip_SA]
  nmult          <- length(pmult)
  list_output    <- vector( "list", nmult )
  for (im in 1:nmult) {
    params[ip_SA]     <- par_SA_default * pmult[im]
    list_output[[im]] <- run_model(params)
  }
  pdf( file_plot, paper="a4r", width=11, height=8 )
  plot_output( list_output, vars=vars,
               leg=as.character(pmult*par_SA_default), leg_title=parname_SA,
               nrow_plot=nrow_plot, ncol_plot=ncol_plot, lty=lty, lwd=lwd )
  dev.off()
  table_output(list_output, vars=vars,
               file_table=paste("SA_",parname_SA,format(Sys.time(),"_%H_%M.txt"),sep=""),
               leg=paste(parname_SA,"=",pmult*par_SA_default,sep=""))
}

##############################
### 5.2 Function 'SA_BC()'
##############################
SA_BC <- function(
  parname_SA    = "KEXT",
  pmult         = 2^(-1:1),
  vars          = outputNames[-(1:3)],
  leg_title     = parname_SA,
  nrow_plot     = ceiling( sqrt((length(vars)+1) * 8/11) ),
  ncol_plot     = ceiling( (length(vars)+1)/nrow_plot ),
  lty           = rep(1,length(pmult)),
  lwd           = rep(3,length(pmult)),
  file_init_BC  = "BC/BC_CAF2014_MCMC_init_Turrialba_1&2.R",
  file_par      = "CAF2014_parModes.txt",
  partype       = "MAP",
  file_plot_outputs      = paste("SA_BC_outputs"     ,format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_outputs_data = paste("SA_BC_outputs_data",format(Sys.time(),"_%H_%M.pdf"),sep=""),
  file_plot_likelihoods  = paste("SA_BC_likelihoods" ,format(Sys.time(),"_%H_%M.pdf"),sep="") )
{ source( file_init_BC )
  nmult <- length(pmult)
  cat( "SA initialised for:", substr(basename(file_init_BC),1,nchar(basename(file_init_BC))-2), "\n")
  parheaders  <- paste( partype, "_", as.character(1:nSites), sep="" )
  df_parModes <- read.table( file_par, header=TRUE, sep="\t" )
  # SITE CONDITIONS
  source('BC/BC_CAF2014_MCMC_init_general.R')
  for (s in 1:nSites) {
    params           <- as.matrix( df_parModes[ parheaders[s] ] )
    list_params[[s]] <- params
  } 
  ip_SA <- match( parname_SA, row.names(df_params) )  
  ivar             <- unique( unlist(data_index) )
  names_var        <- outputNames[ivar]
  nvar             <- length(ivar)
  df_logL          <- data.frame( matrix(nrow=nSites,ncol=nvar) )
  names(df_logL)   <- names_var
  list_df_logL     <- vector("list",nmult)
  for(i in 1:nmult) list_df_logL[[i]] <- df_logL
  
  pdf( file_plot_outputs_data, paper="a4r", width=11, height=8 )
  dev_outputs_data <- dev.cur()    
  pdf( file_plot_outputs     , paper="a4r", width=11, height=8 )
  dev_outputs      <- dev.cur()  
  pdf( file_plot_likelihoods , paper="a4r", width=11, height=8 )
  dev_likelihoods  <- dev.cur()  
  
  for (s in 1:nSites) {    
    params         <- list_params        [[s]] ; matrix_weather <- list_matrix_weather[[s]]
    calendar_fert  <- list_calendar_fert [[s]] ; calendar_prunC <- list_calendar_prunC[[s]] 
    calendar_prunT <- list_calendar_prunT[[s]] ; calendar_thinT <- list_calendar_thinT[[s]]  
    NDAYS          <- list_NDAYS         [[s]]
    par_SA_default <- params[ip_SA]
    list_output    <- vector( "list", nmult )
    for (g in 1:nmult) {
      params[ip_SA]    <- par_SA_default * pmult[g]
      output           <- run_model( params, matrix_weather, calendar_fert, calendar_prunC,
                                     calendar_prunT, calendar_thinT, NDAYS )
      list_output[[g]] <- output        
      for(v in 1:nvar) {
        logLi_s                   <- calc_logLi_s( s, output=output )
        rows_var_s                <- which( logLi_s[,1]==ivar[v] )
        list_df_logL[[g]][[v]][s] <- sum( logLi_s[rows_var_s,2] )
      }
    }        
    dev.set(dev_outputs_data)
    leg_title_outputs_data <- parname_SA
    leg                    <- as.character(pmult*par_SA_default)
    plot_outputs_data_s( isite        = s,
                         list_runs    = list_output,
                         leg_title    = leg_title_outputs_data,
                         leg          = leg,
                         cols         = 1:nmult,
                         lwds         = lwd,
                         ltys         = lty ) 
    dev.set(dev_outputs)
    leg_title_outputs <- paste( "Site: ", s, ", par: ", parname_SA, sep="")
    par( omi=c(0,0,0.3,0), mar=c(2,2,2,1) )
    plot_output( list_output = list_output,
                 vars        = vars,
                 leg_title   = leg_title_outputs,
                 leg         = leg,
                 nrow_plot   = nrow_plot, ncol_plot = ncol_plot,
                 lty         = lty      , lwd       = lwd ) 
    sitenames <- gsub( ".R", "", sub(".*CAF2014_","",sitesettings_filenames) )
    mtext( paste("SITE ",s," (",sitenames[s],"), ","par: ",parname_SA, sep=""),
           side=3, line=1, outer=TRUE, cex=1, font=2)   
  }
  dev.off(dev_outputs_data)
  dev.off(dev_outputs     )
  
  dev.set(dev_likelihoods )
  nrow_plot_L <- ceiling( sqrt((nSites+3) * 8/11) )
  ncol_plot_L <- ceiling( (nSites+3)/nrow_plot_L )
  par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
  
  vector_logLtot <- rep( NA, nmult )
  for(g in 1:nmult) vector_logLtot[g] <- sum( list_df_logL[[g]], na.rm=TRUE )
  barplot( vector_logLtot-min(vector_logLtot),
           main=paste("SUM ALL SITES"), col=1:nmult )
  for(s in 1:nSites) {
    vector_logL <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logL[g] <- rowSums(list_df_logL[[g]]) [s]
    }
    barplot( vector_logL-min(vector_logL), main=paste("site",s),
             ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
             col=1:nmult )
  }
  plot(1,type='n', axes=FALSE, xlab="", ylab="")
  legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )

  for(v in 1:nvar) {
    par( mfrow=c(nrow_plot_L,ncol_plot_L), mar=c(2, 2, 2, 1) )
    vector_logLtot <- rep(NA,nmult)
    for(g in 1:nmult) {
      vector_logLtot[g] <- sum( list_df_logL[[g]][[v]], na.rm=TRUE )
    }
    barplot( vector_logLtot-min(vector_logLtot),
             main=paste(names_var[v],":","SUM ALL SITES"), col=1:nmult )
    vector_logL    <- rep(NA,nmult)
    for (s in 1:nSites) {
      for(g in 1:nmult) {
        vector_logL[g] <- list_df_logL[[g]][[v]] [s]
      }
      barplot( vector_logL-min(vector_logL),
               main=paste(names_var[v],":","site",s),
               ylim=c(0,max(vector_logLtot)-min(vector_logLtot)),
               col=1:nmult )
    }
    plot(1,type='n', axes=FALSE, xlab="", ylab="")
    legend( "bottomright", title=parname_SA, leg=leg, fill=1:nmult )
  }
  dev.off(dev_likelihoods )
} # End of function 'SA_BC()'

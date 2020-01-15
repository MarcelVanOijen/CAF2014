module declare_parameters

! RUN CONTROL AND MATHEMATICAL CONSTANTS
  integer, parameter    :: DELT  = 1
  real   , parameter    :: PI    = ACOS(-1.0)
  
! ENVIRONMENT
  real   , parameter    :: CO20  = 350 ! (ppm) Reference value of [CO2] at which fLUECO2 = 1
  real                  :: CO2A        ! (ppm) Actual value of [CO2]
  real                  :: LAT
  real                  :: IOMULT, RAINMULT, TPLUS

! SOIL
  real                  :: KNFIX, KRUNOFF, RRUNBULK, SLOPE
  real                  :: FWCAD, FWCWP, FWCFC, FWCWET, WCST
  real                  :: CLITT0, CSOM0, CNLITT0, CNSOMF0, CNSOMS0, FCSOMF0, FLITTSOMF, FSOMFSOMS
  real                  :: RNLEACH, KNEMIT, NMIN0, TCLITT, TCSOMF, TCSOMS, TMAXF, TSIGMAF, RFN2O
  real                  :: ROOTD, WFPS50N2O
 
! MANAGEMENT
  real                  :: TREEDENS0
  real                  :: NFERTMULT

! TREES
real :: BETA, CBTREE0, CLTREE0, CRTREE0, CSTREE0, FB, FLMAX, FNCLMINT, FS
real :: FTCCLMINT, GAMMA, KAC, KACEXP, KBA, KEXTT, KH, KHEXP, KNMINT, KNUPTT
real :: KRAININTT, LAI0, LUEMAX, NCLMAXT, NCRT, NCWT, SLAT, TCCBT, TCCLMAXT, TCCRT, TOPTT
real :: TTOLT, TRANCOT, WOODDENS, LAIMAXT, SHADEPROJ, TDIFFMAX

! COFFEE
real :: KSINKPPAR
real :: CL0,CP0,CR0,CW0,DVS0
real :: CCONC,DAYSPLNOP,DAYSPRNOP,KEXT,KFLPAR,KNMIN,KNUPT,KRAININT,NCLMAX,FNCLMIN
real :: NCP,NCR,NCW,RAINdoyHI,RUBISC,SINKL,SINKPMAX,SINKR,SINKW,SLAMAX,FSLAMIN
real :: TCCLMAX,FTCCLMIN,TCCR,TMATB,TMATT,TRANCO,YG
real :: NLAMAX, NLAMIN
real :: FSINKPMAX0, KSINKPMAX
  
end module declare_parameters

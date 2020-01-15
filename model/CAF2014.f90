Subroutine CAF2014(PARAMS,MATRIX_WEATHER, &
                   CALENDAR_FERT,CALENDAR_PRUNC,CALENDAR_PRUNT,CALENDAR_THINT, &
				   NDAYS,NOUT, &
				   y)
!=====================================================
! This is the CAF2014 model, based on CAF2007.
! Authors: Marcel van Oijen, mvano@ceh.ac.uk
!          David Cameron   , dcam@ceh.ac.uk
! Date: 2014-07-18
!=====================================================

use belowgroundres
use coffee
use declare_parameters
use environment
use management
use shade
use soil
use tree
implicit none

! As long as the total number of parameters stays below 120, the next line need not be changed
integer, parameter        :: NPAR     = 120
real                      :: PARAMS(NPAR)
integer, parameter        :: NWEATHER = 8
real                      :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)
real   , dimension(100,3) :: CALENDAR_FERT, CALENDAR_PRUNC, CALENDAR_PRUNT, CALENDAR_THINT
integer, dimension(100,2) :: DAYS_FERT    , DAYS_PRUNC    , DAYS_PRUNT    , DAYS_THINT
real   , dimension(100)   ::                FRPRUNC       , FRPRUNT       , FRTHINT
integer, dimension(100)   :: NFERTV
real                      :: y(NDAYS,NOUT)

integer :: day, doy, i, ic, NDAYS, NOUT, year

! State variables
real    :: CBT  , CLT  , CRT   , CST   , NLT
real    :: CL(2), CP(2), CR(2) , CW(2) , NL(2)    , NCL(2)
real    :: daysinceprun, DVS(2), LAI(2), SENSIT(2), SINKP(2)
real    :: CLITT(2), CSOMF(2), CSOMS(2), NLITT(2) , NMIN(2) , NSOMF(2), NSOMS(2), WA(2)

! Non-state variables
real    :: EvapTree, fTranTree, LAITSA , NsupTree, RainIntTree, RWAT, TranTree
real    :: Evap(2) , fTran(2) , Nsup(2), Tran(2) , RWA(2)
real    :: PARav   , PARint(2), RAINint(2), TCOFFEE(2)
real    :: AVEPAR  , PARCOFFEE(2), SUMPAR(2,30)
real    :: harvDMav

! EXTRA OUTPUT VARIABLES
real    :: harvDMav_year

! PARAMETERS
call set_params(PARAMS)

! Calendar of weather
YEARI  = MATRIX_WEATHER(:,1)
DOYI   = MATRIX_WEATHER(:,2)
GRI    = MATRIX_WEATHER(:,3)
TMINI  = MATRIX_WEATHER(:,4)
TMAXI  = MATRIX_WEATHER(:,5)
VPI    = MATRIX_WEATHER(:,6)
WNI    = MATRIX_WEATHER(:,7)
RAINI  = MATRIX_WEATHER(:,8)

! Calendar of management
DAYS_FERT  = CALENDAR_FERT (:,1:2)
DAYS_PRUNC = CALENDAR_PRUNC(:,1:2)
DAYS_PRUNT = CALENDAR_PRUNT(:,1:2)
DAYS_THINT = CALENDAR_THINT(:,1:2)
NFERTV     = CALENDAR_FERT (:,3)
FRPRUNC    = CALENDAR_PRUNC(:,3)
FRPRUNT    = CALENDAR_PRUNT(:,3)
FRTHINT    = CALENDAR_THINT(:,3)
daysinceprun = 0

! INITIAL STATES

! Trees
treedens = TREEDENS0
CBT      = CBtree0 * TREEDENS0
CLT      = CLtree0 * TREEDENS0
CRT      = CRtree0 * TREEDENS0
CST      = CStree0 * TREEDENS0
NLT      = CLtree0 * TREEDENS0 * NCLMAXT
if (treedens.gt.0) then
  NCLT     = NLT / CLT
else
  NCLT     = 0
end if
! 2014-07-30
SA = min(1., max(0., KAC*(CBtree0**KACEXP)*treedens ) * SHADEPROJ )

! Soil
CLITT = CLITT0
CSOMF = CSOM0 * FCSOMF0
CSOMS = CSOM0 * (1-FCSOMF0)
NLITT = CLITT0 / CNLITT0
NSOMF = (CSOM0 *    FCSOMF0)  / CNSOMF0
NSOMS = (CSOM0 * (1-FCSOMF0)) / CNSOMS0
NMIN  = NMIN0
WA    = 1000 * ROOTD * WCST * FWCFC

! Coffee
DVS           = DVS0
CL            = CL0
CP            = CP0
CR            = CR0
CW            = CW0
NL            = CL * NCLMAX
LAI           = CL * SLAMAX
if (treedens.eq.0) then
  CL(2)       = 0
  CP(2)       = 0
  CR(2)       = 0
  CW(2)       = 0
  NL(2)       = 0
  LAI(2)      = 0
end if
SENSIT        = 0
SINKP         = 0
SINKPMAXnew   = FSINKPMAX0 * SINKPMAX
SUMPAR        = 0
harvDMav_year = 0

do day = 1, NDAYS

! Environment
  call set_weather_day(day, year,doy)
! Trees  
  call morphology(CLT,CST,CBT,treedens,LAIT)
! Management
  call fert_prune_thin(year,doy,DAYS_FERT ,NFERTV ,DAYS_PRUNC,FRPRUNC, &
                                DAYS_PRUNT,FRPRUNT,DAYS_THINT,FRTHINT)
  treedens     = treedens - thintreedens
! Update fluxes
  call CalcShade(LAI,CR,CW,CP,CL,NL,WA,CLITT,CSOMF,CSOMS,NLITT,NSOMF,NSOMS,NMIN)
  if (treedens.gt.0) then
     LAITSA = (LAIT*CA) / SA
  else
     LAITSA = 0
  end if
  RAINintTree  = min(RAIN,KRAININTT*LAITSA)                
  call PETtr(LAITSA,RAINintTree,T,GR)
  call water_flux(WA(2),EvapTree,TranTree,TRANCOT,fTranTree,RWAT)
  call NPP(fTranTree)
  if (treedens.gt.0) then
    PARSHADE     = PAR - (PARabsCrown * CA)/SA
    TSHADE       = T   - TDIFFMAX*(1.0 - PARSHADE/PAR)
  else
    PARSHADE     = PAR
    TSHADE       = T
  endif
  call Nsupplytree(CRT,NMIN,NsupTree)
! Trees continued
  call allocation(fTranTree,LAIT)
  call NdemandOrgans
  call gtreeNupt(NsupTree,SA)
  call CNtree(fTranTree,CRT,CST,CBT,CLT,NLT)
! Environment continued
  call DDAYL(doy)
! Coffee
  TCOFFEE(1)   = T
  TCOFFEE(2)   = TSHADE
  PARCOFFEE(1) = PAR
  PARCOFFEE(2) = PARSHADE
  RAINint(1)   = min(RAIN            ,KRAININT*LAI(1))                     
  RAINint(2)   = min(RAIN-RAINintTree,KRAININT*LAI(2))                  
  do ic = 1,2
    AVEPAR = 0.
    do i=1,30
      AVEPAR = AVEPAR + SUMPAR(ic,i)/30.
    enddo
    do i=30,2,-1
      SUMPAR(ic,i) = SUMPAR(ic,i-1)
    enddo
    SUMPAR(ic,1) = PARCOFFEE(ic)
    call PETtr(LAI(ic),RAINint(ic),TCOFFEE(ic),2*PARCOFFEE(ic))
    call water_flux(WA(ic),Evap(ic),Tran(ic),TRANCO,fTran(ic),RWA(ic))
    call Nsupply(CR,NMIN,Nsup,ic)
    call abovegroundres(LAI,PARCOFFEE,PARav,PARint,ic)
    call Phenology(Day,doy,DVS,SENSIT,TCOFFEE,ic,SINKP)
    call growth(TCOFFEE,PARav,PARint,daysinceprun,fTran,SINKP,Nsup,AVEPAR,DVS,LAI,ic)
    call Foliage(fTran,ic)  
    call Senescence(CR,NL,CL,LAI,fTran,ic)
    call PrunHarv(NL,CL,CW,CP,LAI,DVS,ic)
  enddo
! Soil continued 
  call water(WA(1),RAINint(1),Evap(1),Tran(1),LAI(1),RUNOFF(1),Drain(1))
  call water(WA(2),RAINint(2)+RAINintTree,Evap(2),Tran(2)+TranTree,LAI(2)+LAIT,RUNOFF(2),Drain(2))
  call CNsoil(RWA,WA,gCRT,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS,1) 
  call CNsoil(RWA,WA,gCRT,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS,2) 

! STATE EQUATIONS

! Coffee
if (DVS(1).lt.1.0) then
  SINKP(1) = SINKP(1) + gsink(1)
else
  SINKP(1) = 0
endif 
if (DVS(2).lt.1.0) then
  SINKP(2) = SINKP(2) + gsink(2)
else
  SINKP(2) = 0
endif 
! Sun and shade coffee plants pruned at the same time
daysinceprun = max( 0., daysinceprun + PRUN - 1/DAYSPRNOP )
do ic = 1,2
  LAI(ic)    = LAI(ic)    + adjLAI(ic)   + gLAI(ic)   - dLAI(ic) - prunLAI(ic)
  NL(ic)     = NL(ic)     + adjNL(ic)    + gNL(ic)    - dNL(ic)  - prunNL(ic)
  CL(ic)     = CL(ic)     + adjCL(ic)    + gCL(ic)    - dCL(ic)  - prunCL(ic)  
  CW(ic)     = CW(ic)     + adjCW(ic)    + gCW(ic)               - prunCW(ic)
  CR(ic)     = CR(ic)     + adjCR(ic)    + gCR(ic)    - dCR(ic)
  CP(ic)     = CP(ic)     + adjCP(ic)    + gCP(ic)               - harvCP(ic)
  SENSIT(ic) = SENSIT(ic) + dSENSIT(ic)
  DVS(ic)    = DVS(ic)    + dDVS(ic)     - rDVS(ic)
enddo 

! Trees
  CRT = CRT + gCRT - dCRT
  CST = CST + gCST - dCST
  CBT = CBT + gCBT - dCBT
  CLT = CLT + gCLT - dCLT
  NLT = NLT + gNLT - dNLT

! Soil sun 
  WA(1)    = WA(1)    + adjWA(1)    + RAIN - RAINint(1) - Runoff(1) - Drain(1) - Evap(1) - Tran(1)
  CLITT(1) = CLITT(1) + adjCLITT(1) + dCL(1) - rCLITT(1) - dCLITT(1) + prunCL(1) + prunCW(1)
  CSOMF(1) = CSOMF(1) + adjCSOMF(1) + dCLITTsomf(1) + dCR(1) - rCSOMF(1) - dCSOMF(1)
  CSOMS(1) = CSOMS(1) + adjCSOMS(1) + dCSOMFsoms(1) - dCSOMS(1)
  NLITT(1) = NLITT(1) + adjNLITT(1) + dNL(1)+ prunNL(1) + prunCW(1)*NCW - rNLITT(1) - dNLITT(1)
  NSOMF(1) = NSOMF(1) + adjNSOMF(1) + dCR(1)*NCR + NLITTsomf(1) - rNSOMF(1) - dNSOMF(1)
  NSOMS(1) = NSOMS(1) + adjNSOMS(1) + NSOMFsoms(1) - dNSOMS(1)
  NMIN(1)  = NMIN(1)  + adjNMIN(1)  + Nfert + Nmineralisation(1) - Nupt(1) - Nleaching(1) - Nemission(1)

! Soil shade 
  if (treedens.gt.0) then
    WA(2)    = WA(2)    + adjWA(2)    + RAIN - RAINint(2) - RAINintTree - Runoff(2) - Drain(2) - Evap(2) - Tran(2) - TranTree
    CLITT(2) = CLITT(2) + adjCLITT(2) + dCL(2) + dCLT/SA + dCBT/SA - rCLITT(2) - dCLITT(2) + prunCL(2) + prunCW(2)
    CSOMF(2) = CSOMF(2) + adjCSOMF(2) + dCLITTsomf(2) + dCR(2) + dCRT/SA - rCSOMF(2) - dCSOMF(2)
    CSOMS(2) = CSOMS(2) + adjCSOMS(2) + dCSOMFsoms(2) - dCSOMS(2)
    NLITT(2) = NLITT(2) + adjNLITT(2) + dNLT/SA+dNBlitt/SA+dNL(2) + prunNL(2) + prunCW(2)*NCW - rNLITT(2) - dNLITT(2)
    NSOMF(2) = NSOMF(2) + adjNSOMF(2) + dCR(2)*NCR+ dNRsomf/SA + NLITTsomf(2) - rNSOMF(2) - dNSOMF(2)
    NSOMS(2) = NSOMS(2) + adjNSOMS(2) + NSOMFsoms(2) - dNSOMS(2)
    NMIN(2)  = NMIN(2)  + adjNMIN(2)  + Nfert + Nmineralisation(2) + Nfixation - Nupt(2) - NuptTree/SA - Nleaching(2) - Nemission(2)
  end if
  
! Trees continued  
  call CNtree_N(CLT,NLT)
 
! Outputs
  y(day, 1) = year + (doy-0.5)/366          ! "Time" = Decimal year (approximation)
  y(day, 2) = year
  y(day, 3) = doy
  y(day, 4) = SA                            ! -
  y(day, 5) = h                             ! m       
  y(day, 6) = LAIT                          ! m2  m-2 shade
  y(day, 7) = LAI(1)                        ! m2  m-2 sun
  y(day, 8) = LAI(2)                        ! m2  m-2 shade
  y(day, 9) = harvCP(1)                     ! kgC m-2 sun 
  y(day,10) = harvCP(2)                     ! kgC m-2 shade
    harvDMav      = ((1-SA)*harvCP(1) + SA*harvCP(2)) * 10. / CCONC ! t DM ha-1
	if (doy==61) then
	  harvDMav_year = 0
	else
	  harvDMav_year = harvDMav_year + harvDMav
	end if
  y(day,11) = harvDMav_year                 ! t DM ha-1
  y(day,12) = WA(1)                         ! kgH2O m-2 sun
  y(day,13) = WA(2)                         ! kgH2O m-2 shade
  y(day,14) = treedens                      ! m-2
   
end do ! end time loop

end  

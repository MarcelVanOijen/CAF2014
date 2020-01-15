module coffee

use declare_parameters
use environment
use management
use shade
implicit none

real :: gCW(2),gCP(2),gCL(2),gNL(2),gCR(2),gLAI(2),gsink(2)
real :: dCL(2),dCR(2),dLAI(2),dNL(2),Nupt(2)
real :: prunLAI(2),prunNL(2),prunCL(2),prunCW(2),harvCP(2),harvNP(2)
real :: dSENSIT(2),dDVS(2),rDVS(2),DayFl(2)
real :: SINKPMAXnew(2)

! ALTERNATIVE FLOWERING CALCULATION
real               :: RAINdoy

Contains

  Subroutine Phenology(Day,doy,DVS,SENSIT,TCOFFEE,ic,SINKP)
  integer :: Day,doy,ic
  integer :: DayFill
  real    :: dDVSMAX,DVS(2),SENSIT(2),TCOFFEE(2)
  real    :: SINKP(2)
! Determine whether beans are filling
  if ((DVS(ic).gt.0.0) .and. (DVS(ic).lt.1.0)) then
    DayFill = 1  
  else
    DayFill = 0
  end if
! Making the crop sensitive (to rainfall) in the beginning of the year
  if (DayFill.eq.1) then
    dSENSIT(ic) = -SENSIT(ic)/DELT  
  elseif ((DOY.eq.365).and.(DAY.gt.DAYSPLNOP)) then
    dSENSIT(ic) = 1
  else
    dSENSIT(ic) = 0
  end if
  
! Triggering flowering (CAF2014: this is just a minor improvement on the 2007-version, further development needed)
  RAINdoy = RAIN * doy
  if ((DVS(ic).eq.0.0).and.(RAINdoy.gt.RAINdoyHI)) then
    DayFl(ic) = SENSIT(ic)
  else  
    DayFl(ic) = 0
  end if
  
! Development rate
  if ((DayFill.eq.1).or.(DayFl(ic).eq.1)) then
    dDVSMAX = max(0.0, min(1.0, (TCOFFEE(ic) - TMATB) / TMATT ) )
  else
    dDVSMAX = 0
  end if
  if ((DOY.gt.360).and.(dDVSMAX.gt.0)) then
    dDVS(ic) = (1-DVS(ic))/DELT
  else
    dDVS(ic) = dDVSMAX
  end if
! Development-resetting
  if (DVS(ic).ge.1) then
    rDVS(ic) = DVS(ic)/DELT
	SINKPMAXnew(ic) = SINKPMAX - KSINKPMAX * SINKP(ic)
  else
    rDVS(ic) = 0
  end if
  end Subroutine Phenology

  Subroutine Growth(TINP,PARav,PARint,daysinceprun,fTran,SINKP,Nsup,AVEPAR,DVS,LAI,ic)
  integer :: ic
  real :: TINP(2),PARav,PARint(2),daysinceprun,fTran(2),SINKP(2),Nsup(2),DVS(2),LAI(2)
  real :: EAVCMX,EAKMC,EAKMO,JMUMOL,KC25,KMC25,KMO25,KOKC,O2,R,CO2I,VCMAX,KMC,KMO,GAMMAX,PMAX,EFF,LUECO2,CCass,gSHsource
  real :: TV1,TV2,TV3,TV4
  real :: AVEPAR,Ndemand,fNgrowth
  real :: FCL,FCW,FCR,FCP,gCLpot,gCWpot,gCRpot
  real :: gCPpot,gNLpot,gNWpot,gNRpot,gNPpot,gNLmax,gNW,gNR,gNP,NCLnew
  ! Source strength
  EAVCMX =  68000.                                                  ! % (J mol-1)
  EAKMC  =  65800.                                                  ! % (J mol-1)
  EAKMO  =   1400.                                                  ! % (J mol-1)
  JMUMOL =      4.56                                               ! % (mol quanta MJ-1 PAR)
  KC25   =    138.                                                  ! % (g CO2 g-1 Rubisco d-1)
  KMC25  =    460.                                                  ! % (ppm CO2)
  KMO25  =     33.                                                  ! % (% O2)
  KOKC   =      0.21                                               ! % (-)
  O2     =     21.                                                  ! % (% O2)
  R      =      8.314                                              ! % (J K-1 mol-1)
  CO2I   = 0.7 * CO2A                                              ! % (ppm CO2)
  VCMAX  = RUBISC * KC25 * exp((1./298.-1./(TINP(ic)+273.))*EAVCMX/R)         ! % (g CO2 m-2 leaf d-1)
  KMC    =         KMC25 * exp((1./298.-1./(TINP(ic)+273.))*EAKMC /R)         ! % (ppm CO2)
  KMO    =         KMO25 * exp((1./298.-1./(TINP(ic)+273.))*EAKMO /R)         ! % (% O2)
  GAMMAX = 0.5 * KOKC * KMC * O2 / KMO                             ! % (ppm CO2)
  PMAX   = VCMAX * (CO2I-GAMMAX) / (CO2I + KMC * (1+O2/KMO))       ! % (g CO2 m-2 leaf d-1)
  EFF    = 44. * JMUMOL/2.1 * (CO2I-GAMMAX)/ (4.5*CO2I+10.5*GAMMAX) ! % (g CO2 MJ-1 PAR)
  LUECO2 = EFF * PMAX / (EFF*KEXT*PARav + PMAX)                       ! % (g CO2 MJ-1 PAR)
  CCass  = LUECO2*0.001*(12./44.) * PARint(ic) * fTran(ic) * NOPRUN 
  gSHsource = CCass * YG
  ! Sink strength
  gsink(ic) = (1 - exp(-KSINKPPAR * AVEPAR)) * DayFl(ic) * SINKPMAXnew(ic)  
  if ((DVS(ic).gt.0.0).and.(DVS(ic).lt.1.0)) then
    TV2 = 2*DVS(ic)
  else
    TV2 = 0.0
  endif
  if (TV2.gt.1.0) TV2 = 1.0
  if (daysinceprun.eq.0) then
    TV3 = 1.0
  else
    TV3 = (1-prunFRC)
  endif
  TV4      = SINKL + SINKW + SINKR * (2 - fTran(ic)) + SINKP(ic) * TV2 * TV3
  FCL      = (SINKL * NOPRUN)/TV4
  FCW      = (SINKW * NOPRUN)/TV4
  FCR      = (SINKR * (2 - fTran(ic)) * NOPRUN)/TV4  
  FCP      = (SINKP(ic) * TV2 * TV3 * NOPRUN)/TV4  
  ! N-demand Organs
  gCLpot   = FCL * gSHsource 
  gCWpot   = FCW * gSHsource
  gCRpot   = FCR * gSHsource
  gCPpot   = FCP * gSHsource
  gNLpot   = gCLpot * NCLMAX
  gNWpot   = gCWpot * NCW
  gNRpot   = gCRpot * NCR
  gNPpot   = gCPpot * NCP    
  Ndemand  = gNLpot + gNWpot + gNRpot + gNPpot
  Nupt(ic) = min(Nsup(ic), Ndemand)
  if (Ndemand.ne.0.0) then 
        fNgrowth = Nupt(ic)/Ndemand
  else 
        fNgrowth = Nupt(ic)
  endif
  if (fNgrowth.gt.1.0) fNgrowth = 1.0
  if (fNgrowth.lt.0.0) fNgrowth = 0.0
  gNLmax  = fNgrowth * gNLpot
  gNP     = fNgrowth * gNPpot
  gNR     = fNgrowth * gNRpot
  gNW     = fNgrowth * gNWpot
  gNL(ic) = gNLmax * fNgrowth
  NCLnew  = (fNCLmin + fNgrowth*(1. - fNCLmin))*NCLmax 
  gCW(ic) = gNW/NCW
  gCP(ic) = gNP/NCP
  gCR(ic) = (gNR + (gNLmax - gNL(ic)))/NCR
  gCL(ic) = min(gNL(ic)/NCLnew,gCLpot)
  end Subroutine Growth  
  
  Subroutine Foliage(fTran,ic)
  integer :: ic
  real    :: SLA
  real    :: fTran(2)
  SLA      = SLAMAX*(FSLAMIN + fTran(ic)*(1- FSLAMIN))
  gLAI(ic) = SLA * gCL(ic)
  end Subroutine Foliage
  
  Subroutine Senescence(CR,NL,CL,LAI,fTran,ic)
  integer ic
  real CR(2),NL(2),CL(2),LAI(2),FTran(2) 
  real TV1
  TV1 = 1 / ((fTran(ic)+FTCCLMIN*(1.-fTran(ic)))*TCCLMAX)
  if(TV1.gt.1.0) TV1 = 1.0
  if(TV1.lt.0.0) TV1 = 0.0
  dCR(ic)  = CR(ic)  / TCCR  
  dNL(ic)  = NL(ic)  * TV1
  dCL(ic)  = CL(ic)  * TV1
  dLAI(ic) = LAI(ic) * TV1
  end Subroutine Senescence  
 
  Subroutine PrunHarv(NL,CL,CW,CP,LAI,DVS,ic)
  integer ic
  real NL(2),CL(2),CW(2),CP(2),LAI(2),DVS(2)
  prunLAI(ic) = (PRUN * prunFRC * LAI(ic))/DELT
  prunNL(ic)  = (PRUN * prunFRC * NL(ic) )/DELT
  prunCL(ic)  = (PRUN * prunFRC * CL(ic) )/DELT
  prunCW(ic)  = (PRUN * prunFRC * CW(ic) )/DELT  
  if (DVS(ic).ge.1.0) then
   harvCP(ic) = CP(ic)/DELT
   harvNP(ic) = harvCP(ic) * NCP  
   adjCP(ic)  = 0.0
  else
   harvCP(ic) = 0.0  
   harvNP(ic) = 0.0
  endif 
  end Subroutine PrunHarv

end module coffee








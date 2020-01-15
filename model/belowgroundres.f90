module belowgroundres

use declare_parameters
use environment
use management
use shade
implicit none

real :: BBRAD, NRADC, NRADS, RLWN
real :: SSLOPE, SVP, WDF
real :: BOLTZM, LHVAP, PSYCH
real :: PENMD, PENMRC, PENMRS, PEvap, PTran
real :: fAvail, FRR, WAAD, WC, WCAD, WCCR, WCFC, WCWP, WCWET, WFPS

Contains

  Subroutine PETtr(LAI,RAINint,TINP,GRINP)
  !=============================================================================
  ! Calculate potential rates of evaporation and transpiration (mm d-1)
  ! Author - Marcel van Oijen (CEH-Edinburgh)
  !=============================================================================
  integer :: day
  real    :: LAI
  real    :: RAINint,TINP,GRINP
  
  BOLTZM = 5.668E-8                                        ! % (J m-2 s-1 K-4)
  LHVAP  = 2.4E6                                           ! % (J kg-1)
  PSYCH  = 0.067                                           ! % (kPA degC-1))
  BBRAD  = BOLTZM * (TINP+273.)**4 * 86400.                ! % (J m-2 d-1)
  SVP    = 0.611 * exp(17.4 * TINP/(TINP+239.))            ! % (kPa)
  SSLOPE = 4158.6 * SVP / (TINP+239.)**2                   ! % (kPA degC-1)
  RLWN   = BBRAD * max(0.,0.55*(1.-VP/SVP))                ! % (J m-2 d-1)
  NRADS  = GRINP*1.E6 * (1.-0.15) - RLWN                   ! % (J m-2 d-1)
  NRADC  = GRINP*1.E6 * (1.-0.25) - RLWN                   ! % (J m-2 d-1)
  PENMRS = NRADS * SSLOPE/(SSLOPE+PSYCH)                   ! % (J m-2 d-1)
  PENMRC = NRADC * SSLOPE/(SSLOPE+PSYCH)                   ! % (J m-2 d-1)
  WDF    = 2.63 * (1.0 + 0.54 * WN)                        ! % (kg m-2 d-1 kPa-1)
  PENMD  = LHVAP * WDF * (SVP-VP) * PSYCH/(SSLOPE+PSYCH)   ! % (J m-2 d-1)

  PEvap  =     exp(-0.5*LAI)  * (PENMRS + PENMD) / LHVAP   ! % (mm d-1)
  PTran  = (1.-exp(-0.5*LAI)) * (PENMRC + PENMD) / LHVAP   ! % (mm d-1)
  PTran  = max( 0., PTran-0.5*RAINint )                    ! % (mm d-1)

  end Subroutine PETtr

  Subroutine water_flux(WA,Evap,Tran,TTRANCO,fTran,RWA)
  !=============================================================================
  ! Calculate rates of evaporation and transpiration (mm d-1), and the
  ! transpiration realisation factor (-)
  ! Author: Marcel van Oijen (CEH-Edinburgh)
  ! Date:   6-11-2005, Modified: 10-7-2012
  !=============================================================================
  real :: WA
  real :: Evap, Tran, fTran, RWA
  real :: RWAevap,TTRANCO
  
  WCAD    = WCST * FWCAD                                            ! % (m3 m-3)
  WCWP    = WCST * FWCWP                                            ! % (m3 m-3)
  WCFC    = WCST * FWCFC                                            ! % (m3 m-3)
  WCWET   = WCST * FWCWET                                           ! % (m3 m-3)

  WC      = 0.001 * WA   / ROOTD                                    ! % (m3 m-3)
  RWA     = max(0., min(1., (WC - WCWP) / (WCFC - WCWP) ) )         ! % (-)
  RWAevap = max(0., min(1., (WC - WCAD) / (WCFC - WCAD) ) )         ! % (-)
  WAAD    = 1000. * WCAD * ROOTD                                    ! % (mm)
      
  Evap    = PEvap * RWAevap                                         ! % (mm d-1)
  WCCR    = WCWP + max( 0.01, PTran/(PTran+TTRANCO) * (WCFC-WCWP) ) ! % (m3 m-3)
  if (WC.gt.WCCR) then
     FRR = max(0., min(1., (WCST-WC)/(WCST-WCWET) ))
  else
     FRR = max(0., min(1., (WC-WCWP)/(WCCR-WCWP)  ))
  end if                                                            ! % (mm mm-1)
  Tran    = PTran * FRR                                             ! % (mm d-1)
  if (Evap+Tran.gt.0) then
     fAvail = min( 1., ((WA-WAAD)/DELT) / (Evap+Tran) )
  else
     fAvail = 0.                                                
  end if                                                            ! % (mm mm-1)
         
  Evap    = Evap * fAvail                                           ! % (mm d-1)
  Tran    = Tran * fAvail                                           ! % (mm d-1)

  if (PTran.gt.0) then
     fTran = Tran / PTran                                           ! % (-)
  else
     fTran = 1.                                                     ! % (-)
  end if

  end Subroutine water_flux
  
  Subroutine Nsupply(CR,NMIN,Nsup,ic)
  integer :: ic
  real :: CR(2), NMIN(2), Nsup(2)
  Nsup(ic) = min(CR(ic)*KNUPT*(NMIN(ic)/(KNMIN+NMIN(ic))),NMIN(ic)/DELT)
  end Subroutine Nsupply

  Subroutine Nsupplytree(CRT,NMIN,NsupTree)
  real :: CRT, CRTSA, NMIN(2), NsupTree
  if (treedens.gt.0) then
    CRTSA = CRT / SA                                           !
  else
    CRTSA = 0                                                     !
  end if
  NsupTree = min(CRTSA*KNUPTT*(NMIN(2)/(KNMINT+NMIN(2))),NMIN(2)/DELT)

  end Subroutine Nsupplytree
  
end module belowgroundres      

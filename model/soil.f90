module soil

use declare_parameters
use environment
use management
use shade
implicit none

real :: WAFC, WAST, WC, WCFC
real :: dCLITT(2), rCLITT(2), rCSOMF(2), Rsoil(2)
real :: dCLITTrsoil, dCLITTsomf(2), dCSOMF(2), dCSOMFrsoil, dCSOMFsoms(2), dCSOMS(2)
real :: Nemission(2), Nfixation, Nleaching(2)
real :: NLITTnmin, NLITTsomf(2), Nmineralisation(2)
real :: dNLITT(2), dNSOMF(2), dNSOMS(2), NSOMFnmin, NSOMFsoms(2), rNLITT(2), rNSOMF(2)
real :: Tsoil, fTsoil, Thist(10)
real :: RUNOFF(2), Drain(2)

Contains

  Subroutine water(WA,RAINint,Evap,Tran,LAI,RUNOFF,Drain)
  !=============================================================================
  ! Calculate rates of runoff and drainage (mm d-1)
  ! Author - Marcel van Oijen (CEH-Edinburgh)
  ! 6-11-2005
  !=============================================================================
  integer :: day
  real    :: Evap, LAI, RAINint, Tran, WA
  real    :: RUNOFF, Drain
  WCFC   = WCST * FWCFC                                          ! % (m3 m-3)
  WC     = 0.001 * WA   / ROOTD                                  ! % (m3 m-3)
  WAFC   = 1000. * WCFC * ROOTD                                  ! % (mm)
  WAST   = 1000. * WCST * ROOTD                                  ! % (mm)
  RUNOFF = (RAIN-RAINint) * sin(atan(SLOPE/100)) * &
                                        exp(-KRUNOFF * LAI)      ! % (mm d-1)
  Drain  = max(0. , (WA-WAFC)/DELT + &
                    RAIN - RAINint - Runoff - Evap - Tran )      ! % (mm d-1)
  end Subroutine water

  Subroutine CNsoil(RWA,WA,gCRT,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS,ic)
  integer :: ic
  real :: CLITT(2), CSOMF(2), CSOMS(2), fN2O, gCRT, NLITT(2), NMIN(2), NSOMF(2), NSOMS(2)
  real :: RWA(2), WA(2)
  fTsoil = 1
  ! C Litter
  rCLITT(ic)     = ((CLITT(ic)*Runoff(ic)) / ROOTD) * RRUNBULK * 0.001
  dCLITT(ic)     =  (CLITT(ic)*fTsoil) / TCLITT
  dCLITTsomf(ic) = FLITTSOMF * dCLITT(ic)
  dCLITTrsoil    = dCLITT(ic) - dCLITTsomf(ic)
  ! C SOM fast
  rCSOMF(ic)     = ((CSOMF(ic)*Runoff(ic)) / ROOTD) * RRUNBULK * 0.001
  dCSOMF(ic)     =  (CSOMF(ic)*fTsoil) / TCSOMF
  dCSOMFsoms(ic) = FSOMFSOMS * dCSOMF(ic)
  dCSOMFrsoil    = dCSOMF(ic) - dCSOMFSOMS(ic)
  ! C SOM slow
  dCSOMS         = (CSOMS(ic)*fTsoil) / TCSOMS
  ! Respiration
  Rsoil(ic)      = dCLITTrsoil + dCSOMFrsoil + dCSOMS(ic)
  ! N Litter
  rNLITT(ic)     = ((NLITT(ic)*Runoff(ic)) / ROOTD) * RRUNBULK * 0.001
  dNLITT(ic)     =  (NLITT(ic)*dCLITT(ic)) / CLITT(ic)
  NLITTsomf(ic)  = dNLITT(ic) * FLITTSOMF
  NLITTnmin      = dNLITT(ic) - NLITTsomf(ic)
  ! N SOM fast
  rNSOMF(ic)     = ((NSOMF(ic)*Runoff(ic)) / ROOTD) * RRUNBULK * 0.001
  dNSOMF(ic)     =  (NSOMF(ic)*dCSOMF(ic)) / CSOMF(ic)
  NSOMFsoms(ic)  = dNSOMF(ic) * FSOMFSOMS
  NSOMFnmin      = dNSOMF(ic) - NSOMFsoms(ic)
  ! N SOM slow
  dNSOMS(ic)     = (NSOMS(ic)*dCSOMS(ic)) / CSOMS(ic)
  ! N mineralisation, fixation, leaching, emission
  Nmineralisation(ic) = NLITTnmin + NSOMFnmin + dNSOMS(ic)
  Nfixation           = gCRT/SA * KNFIX
  Nleaching(ic)       = (NMIN(ic)*RNLEACH*Drain(ic)) / WA(ic)
  Nemission(ic)       =  NMIN(ic) * KNEMIT * RWA(ic)
  end Subroutine CNsoil

end module soil      

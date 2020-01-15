module environment

use declare_parameters
implicit none
integer, parameter :: NMAXDAYS = 10000
real    :: GR, PAR, T, RAIN, WN, VP
integer :: doyi(NMAXDAYS), yeari(NMAXDAYS)
real    :: GRI(NMAXDAYS), TMAXI(NMAXDAYS), TMINI(NMAXDAYS), RAINI(NMAXDAYS), WNI(NMAXDAYS), VPI(NMAXDAYS)
real    :: DAYL

Contains

Subroutine set_weather_day(day, year,doy)
  integer :: day, doy, year
  year  = YEARI(day) ! day of the year (d)
  doy   = DOYI(day)  ! day of the year (d)
  GR    = IOMULT*GRI(day)
  PAR   = GR * 0.5
  T     = (TMINI(day)+TMAXI(day))/2. + TPLUS
  RAIN  = RAINMULT * RAINI(day)
  WN    = WNI(day)
  VP    = VPI(day)
  year  = yeari(day)
end Subroutine set_weather_day

Subroutine DDAYL(doy)
!=============================================================================
! Calculate day length (fractional days; -) from Julian day and latitude
!=============================================================================
  integer :: doy
  real    :: DEC, DECC, RAD
  RAD  = PI / 180.                                                    ! (radians deg-1)
  DEC  = -asin (sin (23.45*RAD)*cos (2.*PI*(doy+10.)/365.))           ! (radians)
  DECC = max(atan(-1./tan(RAD*LAT)),min( atan( 1./tan(RAD*LAT)),DEC)) ! (radians)
  DAYL = 0.5 * ( 1. + 2. * asin(tan(RAD*LAT)*tan(DECC)) / PI )        ! (d d-1)
end Subroutine DDAYL

end module environment






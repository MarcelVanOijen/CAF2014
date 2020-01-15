subroutine abovegroundres(LAI,PARCOFFEE,PARav,PARint,ic)
use declare_parameters
use environment
implicit none
integer :: ic
real    :: LAI(2)
real    :: PARav,PARint(2),PARCOFFEE(2)
PARav      = PARCOFFEE(ic) / DAYL
PARint(ic) = PARCOFFEE(ic)*(1. - exp(-KEXT*LAI(ic)))
end Subroutine abovegroundres

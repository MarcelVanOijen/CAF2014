Module shade

use declare_parameters
use environment
use tree
implicit none

real :: SA,TSHADE,PARSHADE
real :: adjLAI(2), adjCR(2)   , adjCW(2)   , adjCP(2)   , adjCL(2)   , adjNL(2)
real :: adjWA(2) , adjCLITT(2), adjCSOMF(2), adjCSOMS(2), adjNLITT(2), adjNSOMF(2), adjNSOMS(2), adjNMIN(2)

Contains
  Subroutine CalcShade(LAI,CR,CW,CP,CL,NL,WA,CLITT,CSOMF,CSOMS,NLITT,NSOMF,NSOMS,NMIN)
  integer :: ic,ii 
  real :: SAold
  real :: dA(2),dfA(2)
  real :: LAI(2), CR(2), CW(2), CP(2), CL(2), NL(2)
  real :: WA(2), CLITT(2), CSOMF(2), CSOMS(2), NLITT(2), NSOMF(2), NSOMS(2), NMIN(2)
  
  SAold = SA
  SA    = CA * SHADEPROJ
  if (SA.gt.1.0) SA = 1.0
  
  dA(1)   = SAold - SA
  dA(2)   = SA    - SAold
  dfA(1)  = max(0.0, dA(1)/(1.0-SA))
  dfA(2)  = max(0.0, dA(2)/SA)

  ii = 2
  do ic = 1,2
     adjCL(ic)    = dfA(ic)*(CL(ii)    - CL(ic))
     adjCP(ic)    = dfA(ic)*(CP(ii)    - CP(ic))  
     adjCR(ic)    = dfA(ic)*(CR(ii)    - CR(ic))
     adjCW(ic)    = dfA(ic)*(CW(ii)    - CW(ic))
     adjNL(ic)    = dfA(ic)*(NL(ii)    - NL(ic))
     adjLAI(ic)   = dfA(ic)*(LAI(ii)   - LAI(ic))
     adjCLITT(ic) = dfA(ic)*(CLITT(ii) - CLITT(ic))
     adjCSOMF(ic) = dfA(ic)*(CSOMF(ii) - CSOMF(ic))
     adjCSOMS(ic) = dfA(ic)*(CSOMS(ii) - CSOMS(ic))
     adjNLITT(ic) = dfA(ic)*(NLITT(ii) - NLITT(ic))
     adjNMIN(ic)  = dfA(ic)*(NMIN(ii)  - NMIN(ic) )
     adjNSOMF(ic) = dfA(ic)*(NSOMF(ii) - NSOMF(ic))
     adjNSOMS(ic) = dfA(ic)*(NSOMS(ii) - NSOMS(ic))
     adjWA(ic)    = dfA(ic)*(WA(ii)    - WA(ic))

     ii = ii - 1 
  enddo

  end Subroutine CalcShade
end Module shade 
